"""Unification module - Robinson's unification algorithm."""
from .unify import (
    UnificationError,
    occurs_check,
    deref,
    unify,
    unify_variable,
    unify_goals,
    rename_variables,
    unify_verbose,
    test_unification,
)

__all__ = [
    "UnificationError",
    "occurs_check",
    "deref",
    "unify",
    "unify_variable",
    "unify_goals",
    "rename_variables",
    "unify_verbose",
    "test_unification",
]