use std::collections::HashMap;

use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod generator;
lalrpop_mod!(grammar);
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use ast::*;
pub use grammar::*;

/// Parse an IDL file string into a vector of definitions
pub fn parse_idl(input: &str) -> Result<Vec<Definition>, String> {
    let mut defs = grammar::IdlFileParser::new()
        .parse(input)
        .map_err(|e| format!("Parse error: {:?}", e))?;

    fold_numeric_constants(&mut defs);
    // Pos processing AST (Abstract Syntax Tree)
    propagate_enum_values(&mut defs);

    Ok(defs)
}

fn fold_numeric_constants(defs: &mut [Definition]) {
    if defs.is_empty() {
        return;
    }

    let mut folder = NumericConstantFolder::default();
    folder.collect_constants(defs);
    folder.fold_all_constants();
    folder.apply(defs);
}

fn propagate_enum_values(defs: &mut [Definition]) {
    for def in defs {
        match def {
            Definition::EnumDef(enum_def) => assign_enum_members(&mut enum_def.node.enumerators),
            Definition::ModuleDef(module_def) => {
                propagate_enum_values(&mut module_def.node.definitions)
            }
            _ => {}
        }
    }
}

#[derive(Default)]
struct NumericConstantFolder {
    constants: HashMap<Vec<String>, ConstEntry>,
}

impl NumericConstantFolder {
    fn collect_constants(&mut self, defs: &[Definition]) {
        let mut scope = Vec::new();
        self.collect_constants_in_scope(defs, &mut scope);
    }

    fn collect_constants_in_scope(&mut self, defs: &[Definition], scope: &mut Vec<String>) {
        for def in defs {
            match def {
                Definition::ModuleDef(module_def) => {
                    scope.push(module_def.node.name.clone());
                    self.collect_constants_in_scope(&module_def.node.definitions, scope);
                    scope.pop();
                }
                Definition::ConstDef(const_def) => {
                    let mut path = scope.clone();
                    path.push(const_def.node.name.clone());
                    self.constants.entry(path).or_insert_with(|| {
                        ConstEntry::new(scope.clone(), const_def.node.value.clone())
                    });
                }
                _ => {}
            }
        }
    }

    fn fold_all_constants(&mut self) {
        let keys: Vec<Vec<String>> = self.constants.keys().cloned().collect();
        for key in keys {
            self.resolve_constant(&key);
        }
    }

    fn resolve_constant(&mut self, path: &[String]) {
        match self.constants.get(path).map(|entry| entry.state) {
            Some(FoldState::Resolved) | None => return,
            Some(FoldState::Resolving) => return,
            Some(FoldState::Unresolved) => {}
        }

        if let Some(entry) = self.constants.get_mut(path) {
            entry.state = FoldState::Resolving;
        }

        let (value_snapshot, scope_snapshot) = match self.constants.get(path) {
            Some(entry) => (entry.value.clone(), entry.scope.clone()),
            None => return,
        };

        let folded_value = self.try_fold_numeric(&value_snapshot, &scope_snapshot, Some(path));

        if let Some(new_value) = folded_value
            && let Some(entry) = self.constants.get_mut(path)
        {
            entry.value = new_value;
        }

        if let Some(entry) = self.constants.get_mut(path) {
            entry.state = FoldState::Resolved;
        }
    }

    fn resolve_scoped_value(
        &mut self,
        scoped_name: &[String],
        scope: &[String],
        current_path: Option<&[String]>,
    ) -> Option<ConstValue> {
        let target_path = self.find_const_path(scoped_name, scope)?;
        if current_path
            .map(|path| path == target_path.as_slice())
            .unwrap_or(false)
        {
            return None;
        }

        self.resolve_constant(&target_path);
        let value = self.constants.get(&target_path)?.value.clone();
        if Self::is_numeric_literal(&value) {
            Some(value)
        } else {
            None
        }
    }

    fn try_fold_numeric(
        &mut self,
        value: &ConstValue,
        scope: &[String],
        current_path: Option<&[String]>,
    ) -> Option<ConstValue> {
        match value {
            ConstValue::Integer(_) | ConstValue::Float(_) | ConstValue::Fixed(_) => {
                Some(value.clone())
            }
            ConstValue::ScopedName(names) => self.resolve_scoped_value(names, scope, current_path),
            ConstValue::UnaryOp { op, expr } => {
                let inner = self.try_fold_numeric(expr, scope, current_path)?;
                self.apply_unary_op(*op, inner)
            }
            ConstValue::BinaryOp { op, left, right } => {
                let left_value = self.try_fold_numeric(left, scope, current_path)?;
                let right_value = self.try_fold_numeric(right, scope, current_path)?;
                self.apply_binary_op(*op, left_value, right_value)
            }
            _ => None,
        }
    }

    fn apply_unary_op(&self, op: UnaryOperator, value: ConstValue) -> Option<ConstValue> {
        let numeric = NumericValue::from_const(&value)?;
        let result = match op {
            UnaryOperator::Plus => numeric,
            UnaryOperator::Minus => numeric.neg()?,
        };
        Some(result.into_const_value())
    }

    fn apply_binary_op(
        &self,
        op: BinaryOperator,
        left: ConstValue,
        right: ConstValue,
    ) -> Option<ConstValue> {
        let lhs = NumericValue::from_const(&left)?;
        let rhs = NumericValue::from_const(&right)?;
        let result = match op {
            BinaryOperator::Add => lhs.add(rhs)?,
            BinaryOperator::Subtract => lhs.sub(rhs)?,
            BinaryOperator::Multiply => lhs.mul(rhs)?,
            BinaryOperator::Divide => lhs.div(rhs)?,
        };
        Some(result.into_const_value())
    }

    fn find_const_path(&self, scoped_name: &[String], scope: &[String]) -> Option<Vec<String>> {
        if scoped_name.is_empty() {
            return None;
        }

        for prefix in (0..=scope.len()).rev() {
            let mut candidate: Vec<String> = scope[..prefix].to_vec();
            candidate.extend_from_slice(scoped_name);
            if self.constants.contains_key(&candidate) {
                return Some(candidate);
            }
        }

        None
    }

    fn apply(&mut self, defs: &mut [Definition]) {
        let mut scope = Vec::new();
        self.apply_in_scope(defs, &mut scope);
    }

    fn apply_in_scope(&mut self, defs: &mut [Definition], scope: &mut Vec<String>) {
        for def in defs {
            match def {
                Definition::ModuleDef(module_def) => {
                    self.fold_annotations(&mut module_def.annotations, scope);
                    scope.push(module_def.node.name.clone());
                    self.apply_in_scope(&mut module_def.node.definitions, scope);
                    scope.pop();
                }
                Definition::EnumDef(enum_def) => {
                    self.fold_annotations(&mut enum_def.annotations, scope);
                    for member in &mut enum_def.node.enumerators {
                        if let Some(value) = &mut member.value {
                            self.fold_const_value(value, scope);
                        }
                    }
                }
                Definition::StructDef(struct_def) => {
                    self.fold_annotations(&mut struct_def.annotations, scope);
                    for member in &mut struct_def.node.members {
                        self.fold_annotations(&mut member.annotations, scope);
                    }
                }
                Definition::ConstDef(const_def) => {
                    self.fold_annotations(&mut const_def.annotations, scope);
                    let mut path = scope.clone();
                    path.push(const_def.node.name.clone());
                    if let Some(entry) = self.constants.get(&path) {
                        const_def.node.value = entry.value.clone();
                    } else {
                        self.fold_const_value(&mut const_def.node.value, scope);
                    }
                }
                Definition::TypeDef(type_def) => {
                    self.fold_annotations(&mut type_def.annotations, scope);
                }
                Definition::ImportDef(import_def) => {
                    self.fold_annotations(&mut import_def.annotations, scope);
                }
            }
        }
    }

    fn fold_annotations(&mut self, annotations: &mut [Annotation], scope: &[String]) {
        for annotation in annotations {
            for param in &mut annotation.params {
                match param {
                    AnnotationParam::Named { value, .. } => self.fold_const_value(value, scope),
                }
            }
        }
    }

    fn fold_const_value(&mut self, value: &mut ConstValue, scope: &[String]) {
        match value {
            ConstValue::ScopedName(names) => {
                let scoped = names.clone();
                if let Some(resolved) = self.resolve_scoped_value(&scoped, scope, None) {
                    *value = resolved;
                }
            }
            ConstValue::UnaryOp { expr, .. } => {
                self.fold_const_value(expr, scope);
                let snapshot = value.clone();
                if let Some(resolved) = self.try_fold_numeric(&snapshot, scope, None) {
                    *value = resolved;
                }
            }
            ConstValue::BinaryOp { left, right, .. } => {
                self.fold_const_value(left, scope);
                self.fold_const_value(right, scope);
                let snapshot = value.clone();
                if let Some(resolved) = self.try_fold_numeric(&snapshot, scope, None) {
                    *value = resolved;
                }
            }
            _ => {}
        }
    }

    fn is_numeric_literal(value: &ConstValue) -> bool {
        matches!(
            value,
            ConstValue::Integer(_) | ConstValue::Float(_) | ConstValue::Fixed(_)
        )
    }
}

#[derive(Clone)]
struct ConstEntry {
    scope: Vec<String>,
    value: ConstValue,
    state: FoldState,
}

impl ConstEntry {
    fn new(scope: Vec<String>, value: ConstValue) -> Self {
        ConstEntry {
            scope,
            value,
            state: FoldState::Unresolved,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum FoldState {
    Unresolved,
    Resolving,
    Resolved,
}

#[derive(Debug, Clone, Copy)]
enum NumericValue {
    Integer(i64),
    Float(f64),
}

impl NumericValue {
    fn from_const(value: &ConstValue) -> Option<Self> {
        match value {
            ConstValue::Integer(literal) => Some(NumericValue::Integer(literal.value)),
            ConstValue::Float(v) => Some(NumericValue::Float(*v)),
            ConstValue::Fixed(fixed) => Some(NumericValue::Float(fixed.to_f64())),
            _ => None,
        }
    }

    fn neg(self) -> Option<Self> {
        match self {
            NumericValue::Integer(v) => v.checked_neg().map(NumericValue::Integer),
            NumericValue::Float(v) => Some(NumericValue::Float(-v)),
        }
    }

    fn add(self, rhs: Self) -> Option<Self> {
        use NumericValue::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.checked_add(b).map(Integer),
            (Integer(a), Float(b)) => Some(Float(a as f64 + b)),
            (Float(a), Integer(b)) => Some(Float(a + b as f64)),
            (Float(a), Float(b)) => Some(Float(a + b)),
        }
    }

    fn sub(self, rhs: Self) -> Option<Self> {
        use NumericValue::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.checked_sub(b).map(Integer),
            (Integer(a), Float(b)) => Some(Float(a as f64 - b)),
            (Float(a), Integer(b)) => Some(Float(a - b as f64)),
            (Float(a), Float(b)) => Some(Float(a - b)),
        }
    }

    fn mul(self, rhs: Self) -> Option<Self> {
        use NumericValue::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.checked_mul(b).map(Integer),
            (Integer(a), Float(b)) => Some(Float(a as f64 * b)),
            (Float(a), Integer(b)) => Some(Float(a * b as f64)),
            (Float(a), Float(b)) => Some(Float(a * b)),
        }
    }

    fn div(self, rhs: Self) -> Option<Self> {
        use NumericValue::*;
        match (self, rhs) {
            (Integer(_), Integer(0)) => None,
            (Integer(a), Integer(b)) => a.checked_div(b).map(Integer),
            (Integer(a), Float(b)) => {
                if b == 0.0 {
                    None
                } else {
                    Some(Float(a as f64 / b))
                }
            }
            (Float(a), Integer(b)) => {
                if b == 0 {
                    None
                } else {
                    Some(Float(a / b as f64))
                }
            }
            (Float(a), Float(b)) => {
                if b == 0.0 {
                    None
                } else {
                    Some(Float(a / b))
                }
            }
        }
    }

    fn into_const_value(self) -> ConstValue {
        match self {
            NumericValue::Integer(value) => {
                ConstValue::Integer(IntegerLiteral::new(value, IntegerBase::Decimal))
            }
            NumericValue::Float(value) => ConstValue::Float(value),
        }
    }
}

fn assign_enum_members(members: &mut [EnumMember]) {
    let mut next_value = Some(IntegerLiteral::new(0, IntegerBase::Decimal));
    for member in members.iter_mut() {
        match &member.value {
            Some(ConstValue::Integer(integer_literal)) => {
                let mut new_value = integer_literal.clone();
                new_value.value = new_value.value.checked_add(1).unwrap();
                next_value = Some(new_value);
            }
            Some(_) => {
                next_value = None;
            }
            None => {
                if let Some(value) = next_value {
                    member.value = Some(ConstValue::Integer(value.clone()));
                    let mut new_value = value.clone();
                    new_value.value = new_value.value.checked_add(1).unwrap();
                    next_value = Some(new_value);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parses_example_fixture() {
        let defs = load_fixture("example.idl");
        assert!(
            !defs.is_empty(),
            "example fixture should yield at least one definition"
        );
    }

    fn load_fixture(name: &str) -> Vec<Definition> {
        let path = format!("tests/fixtures/{name}");
        let input = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", path, err));
        parse_idl(&input).expect("fixture should parse successfully")
    }
}
