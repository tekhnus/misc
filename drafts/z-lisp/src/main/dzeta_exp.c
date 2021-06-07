/*
 * Implementation of dzeta_exp.h.
 */

#include "dzeta_exp.h"


/*
 * WARNING: values of fields not corresponding to current expression type
 * are undefined.
 */
struct Expression {
    enum ExpressionType type;

    // s-expression
    struct ExpressionList* items;

    // Integer
    int int_value;

    // ID
    const char* string_value;
    size_t string_value_size;
};


struct Expression* expression_create_integer(int int_value) {
    struct Expression* expression = malloc(sizeof(struct Expression));
    //freed at expression_delete
    expression->type = INTEGER;
    expression->int_value = int_value;
    return expression;
}


struct Expression* expression_create_id(const char* string_value,
                                        size_t string_value_size) {
    struct Expression* expression = malloc(sizeof(struct Expression));
    //freed at expression_delete
    expression->type = ID;
    expression->string_value = string_value;
    expression->string_value_size = string_value_size;
    return expression;
}


struct Expression* expression_create(struct ExpressionList* items) {
    struct Expression* expression = malloc(sizeof(struct Expression));
    //freed at expression_delete
    expression->type = S_EXPR;
    expression->items = items;
    return expression;
}


void expression_delete(struct Expression* expression) {
    free(expression);
}


struct ExpressionList {
    struct Expression* expression;
    struct ExpressionList* next;
};


struct ExpressionList* expression_list_create() {
    struct ExpressionList* expression_list = malloc(sizeof(struct ExpressionList));
    // freed at expression_list_delete
    expression_list->expression = NULL;
    expression_list->next = NULL;
    return expression_list;
}


void expression_list_delete(struct ExpressionList* list) {
    if (list != NULL) {
        struct ExpressionList* next = list->next;
        free(list);
        expression_list_delete(next);
    }
}


void expression_list_append(struct ExpressionList* expression_list,
                            struct Expression* expression) {
    struct ExpressionList* next = expression_list_create();
    next->expression = expression;
    expression_list->next = next;
}


struct ExpressionList* expression_list_next(const struct ExpressionList* expression_list) {
    return expression_list->next;
}
