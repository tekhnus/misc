#ifndef DZETA_EXP_H
#define DZETA_EXP_H

/*
 * Syntax tree classes for dzeta-lisp.
 */

#include <stdlib.h>


/*
 * Class for an expression.
 * Expressions can be of two main types:
 *     Atoms like integers, string literals, identifiers;
 *     S-expressions. These are lists of expressions,
 *     surrounded by brackets.
 */
struct Expression;


enum ExpressionType {
    S_EXPR, INTEGER, ID
};


/*
 * Class for operations with expression lists.
 * WARNING: first item is fictive; use expression_list_next
 * to retrieve actual list head. Recommended iteration style:
 *
 * ExpressionList* list = ...;
 * ExpressionList* node = NULL;
 * while((node = expression_list_next(list)) != NULL) {
 *     Expression* expression = node->expression;
 *     ...
 * }
 */
struct ExpressionList;


/*
 * Create an expression for integer literal.
 */
struct Expression* expression_create_integer(int int_value);


/*
 * Create an expression for identifier.
 */
struct Expression* expression_create_id(const char* string_value,
                                        size_t string_value_size);


/*
 * Create s-expression.
 */
struct Expression* expression_create(struct ExpressionList* items);


void expression_delete(struct Expression* expression);


struct ExpressionList* expression_list_create();


void expression_list_delete(struct ExpressionList* list);


void expression_list_append(struct ExpressionList* expression_list,
                            struct Expression* expression);


struct ExpressionList* expression_list_next(const struct ExpressionList* expression_list);


#endif
