use std::rc::Rc;

use indexmap::IndexMap;
use redscript_ast::{self as ast, Span};

use crate::stages::infer::FuncItem;
use crate::symbols::{FieldEntry, Visibility};
use crate::{
    CtxVar, FunctionIndex, FunctionType, Method, MethodFlags, Param, ParamFlags, Type, TypeId,
};

pub fn derive_new_method<'e, 'ctx: 'e>(
    id: TypeId<'ctx>,
    vars: &[Rc<CtxVar<'ctx>>],
    fields: impl IntoIterator<Item = FieldEntry<'e, 'ctx>>,
    span: Span,
) -> Method<'ctx> {
    let flags = MethodFlags::new()
        .with_visibility(Visibility::Public)
        .with_is_final(true)
        .with_is_static(true);
    let return_t = Type::app(
        id,
        vars.iter()
            .map(|var| Type::Ctx(var.clone()))
            .collect::<Rc<[_]>>(),
    );
    let params = fields
        .into_iter()
        .map(|field| {
            Param::new(
                field.name(),
                ParamFlags::new(),
                field.field().type_().clone(),
                field.field().span(),
            )
        })
        .collect::<Box<[_]>>();
    let func_t = FunctionType::new([], params, return_t);
    Method::new(flags, func_t, None, [], Some(span))
}

pub fn derive_new_method_item<'e, 'ctx: 'e>(
    id: FunctionIndex,
    type_name: &'ctx str,
    fields: impl IntoIterator<Item = FieldEntry<'e, 'ctx>> + Clone,
    span: Span,
) -> FuncItem<'static, 'ctx, FunctionIndex> {
    const SELF_NAME: &str = "self";

    let new = ast::Expr::New {
        typ: (
            ast::Type::Named {
                name: type_name,
                args: [].into(),
            },
            span,
        )
            .into(),
        args: [].into(),
    };

    let mut stmts = vec![(
        ast::Stmt::Let {
            name: (SELF_NAME, span),
            typ: None,
            value: Some((new, span).into()),
        },
        span,
    )];

    for field in fields.clone().into_iter() {
        let stmt = ast::Expr::Assign {
            lhs: (
                ast::Expr::Member {
                    expr: (ast::Expr::Ident(SELF_NAME), span).into(),
                    member: field.name(),
                },
                span,
            )
                .into(),
            rhs: (ast::Expr::Ident(field.name()), span).into(),
        };
        stmts.push((ast::Stmt::Expr((stmt, span).into()), span));
    }
    stmts.push((
        ast::Stmt::Return(Some((ast::Expr::Ident(SELF_NAME), span).into())),
        span,
    ));

    let param_names = fields.into_iter().map(|e| e.name()).collect::<Box<[_]>>();
    FuncItem::new(
        id,
        span,
        span,
        param_names,
        ast::FunctionBody::Block(ast::Block::new(stmts)),
        IndexMap::default(),
    )
}
