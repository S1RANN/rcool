use std::fmt::Display;

use crate::string_table::SharedString;

enum Expression {
    Assignment {
        ident: SharedString,
        init: Box<Expression>,
    },
    Dispatch {
        class_ident: Box<Expression>,
        as_type: SharedString,
        method_ident: SharedString,
        params: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        true_do: Box<Expression>,
        else_do: Box<Expression>,
    },
    While {
        condition: Box<Expression>,
        do_expr: Box<Expression>,
    },
    Block(Vec<Expression>),
    Let {
        ident: SharedString,
        ident_type: SharedString,
        init: Option<Box<Expression>>,
        do_expr: Box<Expression>,
    },
    Case {
        condition: Box<Expression>,
        branches: Vec<Branch>,
    },
    New(SharedString),
    IsVoid(SharedString),
    Plus(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Negate(Box<Expression>),
    Less(Box<Expression>, Box<Expression>),
    LessEqual(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    ObjectIdent(SharedString),
    IntLiteral(SharedString),
    StringLiteral(SharedString),
    BoolLiteral(bool),
}

trait TreeFormat {
    fn tree_fmt(&self) -> Vec<String>;
}

impl TreeFormat for Expression {
    fn tree_fmt(&self) -> Vec<String> {
        match self {
            Expression::Assignment { ident, init } => {
                let mut strings = vec![format!("Assignment─┬─ident: {ident}")];

                strings.append(
                    &mut init
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Assignment─┬─ident: "
                                format!("           └─init: {s}")
                            } else {
                                format!("                   {s}")
                            }
                        })
                        .collect(),
                );
                strings
            }
            Expression::Dispatch {
                class_ident,
                as_type,
                method_ident,
                params,
            } => {
                let mut strings: Vec<String> = class_ident
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Dispatch─┬─class_ident: {s}")
                        } else {
                            format!("         │              {s}")
                        }
                    })
                    .collect();

                //                   "Dispatch─┬─class_ident: "
                strings.push(format!("         ├─as_type: {as_type}"));
                strings.push(format!("         ├─method_ident: {method_ident}"));

                strings.append(
                    &mut params
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Dispatch─┬─class_ident: "
                                format!("         └─params: {s}")
                            } else {
                                format!("                   {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::If {
                condition,
                true_do,
                else_do,
            } => {
                let mut strings: Vec<String> = condition
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("If─┬─condition: {s}")
                        } else {
                            format!("   │            {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut true_do
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "If─┬─condition: "
                                format!("   ├─true_do: {s}")
                            } else {
                                format!("   │          {s}")
                            }
                        })
                        .collect(),
                );

                strings.append(
                    &mut else_do
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "If─┬─condition: "
                                format!("   └─else_do: {s}")
                            } else {
                                format!("              {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::While { condition, do_expr } => {
                let mut strings: Vec<String> = condition
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("While─┬─condition: {s}")
                        } else {
                            format!("      │            {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut do_expr
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "While─┬─condition: "
                                format!("      └─do_expr: {s}")
                            } else {
                                format!("                 {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Block(exprs) => exprs
                .tree_fmt()
                .iter_mut()
                .enumerate()
                .map(|(line_idx, s)| {
                    if line_idx == 0 {
                        format!("Block: {s}")
                    } else {
                        format!("       {s}")
                    }
                })
                .collect(),
            Expression::Let {
                ident,
                ident_type,
                init,
                do_expr,
            } => {
                let mut strings = vec![
                    format!("Let─┬─ident: {ident}"),
                    format!("    ├─ident_type: {ident_type}"),
                ];

                if let Some(init_expr) = init {
                    strings.append(
                        &mut init_expr
                            .tree_fmt()
                            .iter_mut()
                            .enumerate()
                            .map(|(line_idx, s)| {
                                if line_idx == 0 {
                                    //      "Let─┬─ident: "
                                    format!("    ├─init: {s}")
                                } else {
                                    format!("    │       {s}")
                                }
                            })
                            .collect(),
                    );
                }

                strings.append(
                    &mut do_expr
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Let─┬─ident: "
                                format!("    └─do_expr: {s}")
                            } else {
                                format!("               {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Case {
                condition,
                branches,
            } => {
                let mut strings: Vec<String> = condition
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Case─┬─condition: {s}")
                        } else {
                            format!("     │            {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut branches
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Case─┬─condition:""
                                format!("     └─branches: {s}")
                            } else {
                                format!("                 {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::New(type_ident) => vec![format!("New───type_ident: {type_ident}")],
            Expression::IsVoid(ident) => vec![format!("IsVoid───ident: {ident}")],
            Expression::Plus(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Plus─┬─lhs: {s}")
                        } else {
                            format!("     │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Plus─┬─lhs: ""
                                format!("     └─rhs: {s}")
                            } else {
                                format!("            {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Subtract(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Subtract─┬─lhs: {s}")
                        } else {
                            format!("         │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Subtract─┬─lhs: ""
                                format!("         └─rhs: {s}")
                            } else {
                                format!("                {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Multiply(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Multiply─┬─lhs: {s}")
                        } else {
                            format!("         │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Multiply─┬─lhs: ""
                                format!("         └─rhs: {s}")
                            } else {
                                format!("                {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Divide(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Divide─┬─lhs: {s}")
                        } else {
                            format!("       │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Divide─┬─lhs: ""
                                format!("       └─rhs: {s}")
                            } else {
                                format!("              {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Negate(ident) => ident
                .tree_fmt()
                .iter_mut()
                .enumerate()
                .map(|(line_idx, s)| {
                    if line_idx == 0 {
                        format!("Negate───ident: {s}")
                    } else {
                        format!("                {s}")
                    }
                })
                .collect(),
            Expression::Less(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Less─┬─lhs: {s}")
                        } else {
                            format!("     │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Less─┬─lhs: ""
                                format!("     └─rhs: {s}")
                            } else {
                                format!("            {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::LessEqual(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("LessEqual─┬─lhs: {s}")
                        } else {
                            format!("          │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "LessEqual─┬─lhs: ""
                                format!("          └─rhs: {s}")
                            } else {
                                format!("                 {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Equal(lhs, rhs) => {
                let mut strings: Vec<String> = lhs
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("Equal─┬─lhs: {s}")
                        } else {
                            format!("      │      {s}")
                        }
                    })
                    .collect();

                strings.append(
                    &mut rhs
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Equal─┬─lhs: ""
                                format!("      └─rhs: {s}")
                            } else {
                                format!("             {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
            Expression::Not(expr) => expr
                .tree_fmt()
                .iter_mut()
                .enumerate()
                .map(|(line_idx, s)| {
                    if line_idx == 0 {
                        format!("Not───ident: {s}")
                    } else {
                        format!("             {s}")
                    }
                })
                .collect(),
            Expression::ObjectIdent(ident) => vec![format!("ObjectIdent: {ident}")],
            Expression::IntLiteral(i) => vec![format!("IntLiteral: {i}")],
            Expression::StringLiteral(s) => vec![format!("StringLiteral: {s}")],
            Expression::BoolLiteral(b) => vec![format!("BoolLiteral: {b}")],
        }
    }
}

impl TreeFormat for Branch {
    fn tree_fmt(&self) -> Vec<String> {
        let mut strings = vec![
            format!("Branch─┬─ident: {}", self.ident),
            format!("       ├─ident_type: {}", self.ident_type),
        ];

        strings.append(
            &mut self
                .do_expr
                .tree_fmt()
                .iter_mut()
                .enumerate()
                .map(|(line_idx, s)| {
                    if line_idx == 0 {
                        //      "Branch─┬─ident: "
                        format!("       └─do_expr: {s}")
                    } else {
                        format!("                  {s}")
                    }
                })
                .collect(),
        );

        strings
    }
}

impl TreeFormat for Formal {
    fn tree_fmt(&self) -> Vec<String> {
        vec![
            format!("Formal─┬─ident: {}", self.ident),
            format!("       └─ident_type: {}", self.ident_type),
        ]
    }
}

impl TreeFormat for Feature {
    fn tree_fmt(&self) -> Vec<String> {
        match self {
            Feature::Attribute {
                ident,
                ident_type,
                init,
            } => {
                if let Some(init_expr) = init {
                    let mut strings = vec![
                        format!("Feature─┬─ident: {ident}"),
                        format!("        ├─ident_type: {ident_type}"),
                    ];

                    strings.append(
                        &mut init_expr
                            .tree_fmt()
                            .iter_mut()
                            .enumerate()
                            .map(|(line_idx, s)| {
                                if line_idx == 0 {
                                    //      "Feature─┬─ident:
                                    format!("        └─init: {s}")
                                } else {
                                    format!("                {s}")
                                }
                            })
                            .collect(),
                    );

                    strings
                } else {
                    vec![
                        format!("Feature─┬─ident: {ident}"),
                        format!("        └─ident_type: {ident_type}"),
                    ]
                }
            }
            Feature::Method {
                ident,
                formals,
                return_type,
                body,
            } => {
                let mut strings = vec![format!("Feature─┬─ident: {ident}")];

                strings.append(
                    &mut formals
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Feature─┬─ident: ""
                                format!("        ├─formals: {s}")
                            } else {
                                format!("        │          {s}")
                            }
                        })
                        .collect(),
                );
                //                   "Feature─┬─ident:
                strings.push(format!("        ├─return_type: {return_type}"));
                strings.append(
                    &mut body
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                //      "Feature─┬─ident: ""
                                format!("        └─body: {s}")
                            } else {
                                format!("                {s}")
                            }
                        })
                        .collect(),
                );

                strings
            }
        }
    }
}

impl<T: TreeFormat> TreeFormat for Vec<T> {
    fn tree_fmt(&self) -> Vec<String> {
        if self.len() == 0 {
            return vec!["───[]".to_string()];
        }
        if self.len() == 1 {
            return self[0]
                .tree_fmt()
                .iter_mut()
                .enumerate()
                .map(|(line_idx, s)| {
                    if line_idx == 0 {
                        format!("───{s}")
                    } else {
                        format!("   {s}")
                    }
                })
                .collect();
        }

        let mut strings = vec![];
        self.iter().enumerate().for_each(|(item_idx, item)| {
            if item_idx == 0 {
                strings.append(
                    &mut item
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                format!("─┬─{s}")
                            } else {
                                format!(" │ {s}")
                            }
                        })
                        .collect(),
                )
            } else if item_idx == self.len() - 1 {
                strings.append(
                    &mut item
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                format!(" └─{s}")
                            } else {
                                format!("   {s}")
                            }
                        })
                        .collect(),
                )
            } else {
                strings.append(
                    &mut item
                        .tree_fmt()
                        .iter_mut()
                        .enumerate()
                        .map(|(line_idx, s)| {
                            if line_idx == 0 {
                                format!(" ├─{s}")
                            } else {
                                format!(" │ {s}")
                            }
                        })
                        .collect(),
                )
            }
        });

        strings
    }
}

impl TreeFormat for Class {
    fn tree_fmt(&self) -> Vec<String> {
        let mut strings = vec![
            format!("Class─┬─class_type: {}", self.class_type),
            format!("      ├─parent: {}", self.parent),
        ];

        strings.append(
            &mut self
                .features
                .tree_fmt()
                .iter_mut()
                .enumerate()
                .map(|(line_idx, s)| {
                    if line_idx == 0 {
                        //      "Class─┬─class_type: "
                        format!("      └─feature: {s}")
                    } else {
                        format!("                 {s}")
                    }
                })
                .collect(),
        );

        strings
    }
}

impl TreeFormat for Program {
    fn tree_fmt(&self) -> Vec<String> {
        self.0
            .tree_fmt()
            .iter_mut()
            .enumerate()
            .map(|(line_idx, s)| {
                if line_idx == 0 {
                    format!("Program: {s}")
                } else {
                    format!("         {s}")
                }
            })
            .collect()
    }
}

struct Branch {
    ident: SharedString,
    ident_type: SharedString,
    do_expr: Box<Expression>,
}

struct Formal {
    ident: SharedString,
    ident_type: SharedString,
}

enum Feature {
    Attribute {
        ident: SharedString,
        ident_type: SharedString,
        init: Option<Box<Expression>>,
    },
    Method {
        ident: SharedString,
        formals: Vec<Formal>,
        return_type: SharedString,
        body: Box<Expression>,
    },
}

struct Class {
    class_type: SharedString,
    parent: SharedString,
    features: Vec<Feature>,
}

struct Program(Vec<Class>);

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tree_fmt().join("\n"))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_program_fmt() {
        let assign_expr = Expression::Assignment {
            ident: SharedString::new("test"),
            init: Box::new(Expression::Plus(
                Box::new(Expression::ObjectIdent(SharedString::new("a"))),
                Box::new(Expression::IntLiteral(SharedString::new("1"))),
            )),
        };
        let let_expr = Expression::Let {
            ident: SharedString::new("x"),
            ident_type: SharedString::new("Double"),
            init: None,
            do_expr: Box::new(Expression::New(SharedString::new("Object"))),
        };
        let features = vec![
            Feature::Attribute {
                ident: SharedString::new("name"),
                ident_type: SharedString::new("String"),
                init: Some(Box::new(assign_expr)),
            },
            Feature::Method {
                ident: SharedString::new("to_string"),
                formals: vec![
                    Formal {
                        ident: SharedString::new("type"),
                        ident_type: SharedString::new("String"),
                    },
                    Formal {
                        ident: SharedString::new("num"),
                        ident_type: SharedString::new("Int"),
                    },
                ],
                return_type: SharedString::new("Void"),
                body: Box::new(let_expr),
            },
        ];
        let class = Class {
            class_type: SharedString::new("TestClass"),
            parent: SharedString::new("IO"),
            features: vec![],
        };
        let class1 = Class {
            class_type: SharedString::new("DummyClass"),
            parent: SharedString::new("Boolean"),
            features,
        };
        let program = Program(vec![class, class1]);
        assert_eq!(
            "Program: ─┬─Class─┬─class_type: TestClass
          │       ├─parent: IO
          │       └─feature: ───[]
          └─Class─┬─class_type: DummyClass
                  ├─parent: Boolean
                  └─feature: ─┬─Feature─┬─ident: name
                              │         ├─ident_type: String
                              │         └─init: Assignment─┬─ident: test
                              │                            └─init: Plus─┬─lhs: ObjectIdent: a
                              │                                         └─rhs: IntLiteral: 1
                              └─Feature─┬─ident: to_string
                                        ├─formals: ─┬─Formal─┬─ident: type
                                        │           │        └─ident_type: String
                                        │           └─Formal─┬─ident: num
                                        │                    └─ident_type: Int
                                        ├─return_type: Void
                                        └─body: Let─┬─ident: x
                                                    ├─ident_type: Double
                                                    └─do_expr: New───type_ident: Object",
            format!("{program}")
        );
    }
}
