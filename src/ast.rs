use std::fmt::Display;

use crate::string_table::SharedString;

#[derive(Debug, PartialEq)]
pub(crate) enum Expression {
    Assignment {
        ident: SharedString,
        init: Box<Expression>,
    },
    Dispatch {
        class_ident: Option<Box<Expression>>,
        as_type: SharedString,
        method_ident: SharedString,
        params: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_do: Box<Expression>,
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
    IsVoid(Box<Expression>),
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

enum ExpressionPrime {
    Dispatch {
        as_type: SharedString,
        method_ident: SharedString,
        params: Vec<Expression>,
        prime: Box<ExpressionPrime>,
    },
    Plus(Box<Expression>, Box<ExpressionPrime>),
    Subtract(Box<Expression>, Box<ExpressionPrime>),
    Multiply(Box<Expression>, Box<ExpressionPrime>),
    Divide(Box<Expression>, Box<ExpressionPrime>),
    Less(Box<Expression>, Box<ExpressionPrime>),
    LessEqual(Box<Expression>, Box<ExpressionPrime>),
    Equal(Box<Expression>, Box<ExpressionPrime>),
    Nil,
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
                let mut strings: Vec<String> = match class_ident {
                    Some(ident) => ident
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
                        .collect(),
                    None => vec![format!("Dispatch─┬─class_ident: None")],
                };

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
                then_do,
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
                    &mut then_do
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
            Expression::IsVoid(expr) => {
                let mut strings: Vec<String> = expr
                    .tree_fmt()
                    .iter_mut()
                    .enumerate()
                    .map(|(line_idx, s)| {
                        if line_idx == 0 {
                            format!("IsVoid───expr: {s}")
                        } else {
                            format!("               {s}")
                        }
                    })
                    .collect();

                strings
            }
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
                        format!("Negate───expr: {s}")
                    } else {
                        format!("               {s}")
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
                        format!("Not───expr: {s}")
                    } else {
                        format!("            {s}")
                    }
                })
                .collect(),
            Expression::ObjectIdent(ident) => vec![format!("ObjectIdent: {ident}")],
            Expression::IntLiteral(i) => vec![format!("IntLiteral: {i}")],
            Expression::StringLiteral(s) => {
                vec![format!("StringLiteral: \"{}\"", s.replace('\n', "\\n"))]
            }
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
        if self.is_empty() {
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
            format!("Class─┬─name: {}", self.name),
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

#[derive(Debug, PartialEq)]
pub(crate) struct Branch {
    pub(crate) ident: SharedString,
    pub(crate) ident_type: SharedString,
    pub(crate) do_expr: Expression,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Formal {
    pub(crate) ident: SharedString,
    pub(crate) ident_type: SharedString,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Feature {
    Attribute {
        ident: SharedString,
        ident_type: SharedString,
        init: Option<Expression>,
    },
    Method {
        ident: SharedString,
        formals: Vec<Formal>,
        return_type: SharedString,
        body: Expression,
    },
}

#[derive(Debug, PartialEq)]
pub(crate) struct Class {
    pub(crate) name: SharedString,
    pub(crate) parent: SharedString,
    pub(crate) features: Vec<Feature>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Program(pub(crate) Vec<Class>);

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tree_fmt().join("\n"))
    }
}

impl Display for Feature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tree_fmt().join("\n"))
    }
}

impl Display for Expression {
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
                init: Some(assign_expr),
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
                body: let_expr,
            },
        ];
        let class = Class {
            name: SharedString::new("TestClass"),
            parent: SharedString::new("IO"),
            features: vec![],
        };
        let class1 = Class {
            name: SharedString::new("DummyClass"),
            parent: SharedString::new("Boolean"),
            features,
        };
        let program = Program(vec![class, class1]);
        assert_eq!(
            "Program: ─┬─Class─┬─name: TestClass
          │       ├─parent: IO
          │       └─feature: ───[]
          └─Class─┬─name: DummyClass
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
