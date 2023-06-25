use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Class, Expression, Feature, Program},
    string_table::{SharedString, StrTable},
};
struct Analyzer<'a> {
    ast: &'a Program,
    classes: HashMap<SharedString, &'a Class>,
    primitive: Primitive,
}

struct Primitive {
    object: SharedString,
    int: SharedString,
    string: SharedString,
    bool: SharedString,
    self_type: SharedString,
    abort: SharedString,
    type_name: SharedString,
    copy: SharedString,
    io: SharedString,
    out_string: SharedString,
    out_int: SharedString,
    in_string: SharedString,
    in_int: SharedString,
    length: SharedString,
    concat: SharedString,
    substr: SharedString,
    val: SharedString,
    str_field: SharedString,
    no_class: SharedString,
    prim_slot: SharedString,
}

struct ClassNode<'a> {
    ref_ast: &'a Class,
    parent: Option<Rc<ClassNode<'a>>>,
}

enum AnalyticalError {
    CyclicInheritance,
    DefineBasicClass,
    RedefinedClass,
    UndefinedClass,
    UndefinedMethod,
    UndefinedVariable,
    TypeMismatch,
    ArgumentMismatch,
    Other,
}

impl<'a> ClassNode<'a> {
    fn new(ref_ast: &'a Class) -> Self {
        ClassNode {
            ref_ast,
            parent: None,
        }
    }
}

impl Primitive {
    fn new(str_table: &mut StrTable) -> Self {
        Primitive {
            object: str_table.insert("object"),
            int: str_table.insert("int"),
            string: str_table.insert("string"),
            bool: str_table.insert("bool"),
            self_type: str_table.insert("self_type"),
            abort: str_table.insert("abort"),
            type_name: str_table.insert("type_name"),
            copy: str_table.insert("copy"),
            io: str_table.insert("io"),
            out_string: str_table.insert("out_string"),
            out_int: str_table.insert("out_int"),
            in_string: str_table.insert("in_string"),
            in_int: str_table.insert("in_int"),
            length: str_table.insert("length"),
            concat: str_table.insert("concat"),
            substr: str_table.insert("substr"),
            val: str_table.insert("_val"),
            str_field: str_table.insert("_str_field"),
            no_class: str_table.insert("__no_class__"),
            prim_slot: str_table.insert("__prim_slot__"),
        }
    }
}

impl<'a> Analyzer<'a> {
    pub(crate) fn new(ast: &'a Program, primitive: Primitive) -> Self {
        Analyzer {
            ast,
            classes: HashMap::new(),
            primitive
        }
    }

    fn add_primitive(ast: &mut Program, primitive: &Primitive) -> Result<(), AnalyticalError> {
        for class in ast.0.iter() {
            if class.name == "Object"
                || class.name == "Int"
                || class.name == "String"
                || class.name == "Bool"
            {
                return Err(AnalyticalError::DefineBasicClass);
            }
        }

        let object_class = Class {
            name: primitive.object.clone(),
            parent: primitive.no_class.clone(),
            features: vec![
                Feature::Method {
                    ident: primitive.abort.clone(),
                    formals: vec![],
                    return_type: primitive.object.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident: primitive.type_name.clone(),
                    formals: vec![],
                    return_type: primitive.string.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident: primitive.copy.clone(),
                    formals: vec![],
                    return_type: primitive.self_type.clone(),
                    body: Expression::NoExpr,
                },
            ],
        };

        let io_class = Class {
            name: primitive.io.clone(),
            parent: primitive.object.clone(),
            features: vec![
                Feature::Method {
                    ident: primitive.out_string.clone(),
                    formals: vec![],
                    return_type: primitive.self_type.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident: primitive.out_int.clone(),
                    formals: vec![],
                    return_type: primitive.self_type.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident: primitive.in_string.clone(),
                    formals: vec![],
                    return_type: primitive.string.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident:primitive.in_int.clone(),
                    formals: vec![],
                    return_type: primitive.int.clone(),
                    body: Expression::NoExpr,
                },
            ],
        };

        let int_class = Class {
            name: primitive.int.clone(),
            parent: primitive.object.clone(),
            features: vec![Feature::Attribute {
                ident: primitive.val.clone(),
                ident_type: primitive.prim_slot.clone(),
                init: None,
            }],
        };

        let bool_class = Class {
            name: primitive.bool.clone(),
            parent: primitive.object.clone(),
            features: vec![Feature::Attribute {
                ident: primitive.val.clone(),
                ident_type: primitive.prim_slot.clone(),
                init: None,
            }],
        };

        let string_class = Class {
            name: primitive.string.clone(),
            parent: primitive.object.clone(),
            features: vec![
                Feature::Attribute {
                    ident: primitive.val.clone(),
                    ident_type: primitive.prim_slot.clone(),
                    init: None,
                },
                Feature::Attribute {
                    ident: primitive.str_field.clone(),
                    ident_type: primitive.prim_slot.clone(),
                    init: None,
                },
                Feature::Method {
                    ident: primitive.length.clone(),
                    formals: vec![],
                    return_type: primitive.int.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident: primitive.concat.clone(),
                    formals: vec![],
                    return_type: primitive.string.clone(),
                    body: Expression::NoExpr,
                },
                Feature::Method {
                    ident: primitive.substr.clone(),
                    formals: vec![],
                    return_type: primitive.string.clone(),
                    body: Expression::NoExpr,
                },
            ],
        };

        ast.0.push(object_class);
        ast.0.push(io_class);
        ast.0.push(int_class);
        ast.0.push(bool_class);
        ast.0.push(string_class);

        Ok(())
    }

    fn build_inheritance(&mut self) -> Result<(), AnalyticalError> {
        for class in self.ast.0.iter() {
            if self.classes.contains_key(&class.name) {
                return Err(AnalyticalError::RedefinedClass);
            }
            self.classes.insert(class.name.clone(), class);
        }

        Ok(())
    }

    fn check_inheritance(&self) -> Result<(), AnalyticalError> {
        for class in self.classes.values() {
            if class.parent == self.primitive.no_class {
                continue;
            }
            if !self.classes.contains_key(&class.parent) {
                return Err(AnalyticalError::UndefinedClass);
            }
        }

        let mut visited = HashMap::new();
        let mut rec = HashMap::new();
        for class in self.classes.keys() {
            visited.insert(class.clone(), false);
            rec.insert(class.clone(), false);
        }

        for class in self.classes.keys() {
            if !visited[class] && self.has_cycle(class, &mut visited, &mut rec) {
                return Err(AnalyticalError::CyclicInheritance);
            }
        }

        Ok(())
    }

    fn has_cycle(
        &self,
        node: &SharedString,
        visited: &mut HashMap<SharedString, bool>,
        rec: &mut HashMap<SharedString, bool>,
    ) -> bool {
        *visited.get_mut(node).unwrap() = true;
        *rec.get_mut(node).unwrap() = true;

        let class = self.classes.get(node).unwrap();
        if class.parent != self.primitive.no_class {
            if !visited[&class.parent] && self.has_cycle(&class.parent, visited, rec) {
                return true;
            } else if rec[&class.parent] {
                return true;
            }
        }

        *rec.get_mut(node).unwrap() = false;

        false
    }

    fn analyze(ast: &mut Program, str_table: &mut StrTable) -> Result<(), AnalyticalError> {
        let primitive = Primitive::new(str_table);
        Self::add_primitive(ast, &primitive)?;
        let mut analyzer = Analyzer::new(ast, primitive);
        analyzer.build_inheritance()?;
        analyzer.check_inheritance()?;
        
        Ok(())
    }
}
