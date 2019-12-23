// force-host
#![allow(deprecated)]
#![feature(plugin_registrar)]
#![feature(box_syntax, rustc_private)]

extern crate syntax;

// Load rustc as a plugin to get macros
#[macro_use]
extern crate rustc;
#[macro_use]
extern crate rustc_session;
extern crate rustc_driver;
extern crate rustc_errors;

use rustc::hir;
use rustc::lint::{LateContext, LateLintPass, LintArray, LintContext, LintPass};
use rustc::ty::TyKind;
use rustc_driver::plugin::Registry;
use rustc_errors::Applicability;

declare_lint!(EXPR_LINT, Warn, "find where expr is used");

declare_lint_pass!(Pass => [EXPR_LINT]);

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for Pass {
    fn check_expr(&mut self, cx: &LateContext<'a, 'tcx>, expr: &'tcx hir::Expr) {
        // find Box::new(expr: hir::Expr)
        let box_span = expr.span;
        if let hir::ExprKind::Call(fun, args) = &expr.kind {
            if args.len() < 1 {
                return;
            }
            let fun_hir_id = fun.hir_id;
            if let hir::ExprKind::Path(fun) = &fun.kind {
                let res = cx.tables.qpath_res(fun, fun_hir_id);
                if let Some(defid) = res.opt_def_id() {
                    let funpath = cx.tcx.def_path_str(defid);
                    if funpath.as_str() == "std::boxed::Box::<T>::new" {
                        let expr = &args[0];
                        if let TyKind::Adt(def, _) = cx.tables.expr_ty(expr).kind {
                            let name = cx.tcx.def_path_str(def.did);
                            if name.as_str() == "ast::Expr" {
                                // Box::new(
                                let box_start = box_span.until(expr.span);
                                // )
                                let box_end = box_span.trim_start(expr.span).unwrap();
                                let expr_str =
                                    cx.tcx.sess.source_map().span_to_snippet(expr.span).unwrap();
                                let mut diag = cx.struct_span_lint(
                                    EXPR_LINT,
                                    box_span,
                                    "using Box::new() is not recommended",
                                );
                                diag.span_suggestion(
                                    box_span,
                                    "you can use .boxed()",
                                    format!("{}.boxed()", expr_str),
                                    Applicability::MachineApplicable,
                                )
                                .emit()
                            }
                        }
                    }
                }
            }
        }
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.lint_store.register_lints(&[&EXPR_LINT]);
    reg.lint_store.register_late_pass(|| box Pass);
}
