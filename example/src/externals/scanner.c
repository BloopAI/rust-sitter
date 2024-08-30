#include <tree_sitter/parser.h>
#include <ctype.h>
#include <string.h>
#include "token_types.h" // Generated at compile-time from the grammar definition

struct context {
    // fields here
};

void *tree_sitter_externals_external_scanner_create() {
    return calloc(1, sizeof(struct context));
}

bool is_white_space(int c) {
    return isspace(c) || c == ';' || c == ',';
}

bool tree_sitter_externals_external_scanner_scan(
    void *payload, 
    TSLexer *lexer,
    const bool *valid_symbols
) {
    if(lexer->eof(lexer)) {
        return false;
    }

    if(valid_symbols[WhiteSpaces]) {
        if(is_white_space(lexer->lookahead)) {
            while(is_white_space(lexer->lookahead)) {
                lexer->advance(lexer, true);
            }
            lexer->result_symbol = WhiteSpaces;
            lexer->mark_end(lexer);
            return true;
        }
    }

    return false;
}

unsigned tree_sitter_externals_external_scanner_serialize(void *payload, char *buffer) {
    return 0;
}

void tree_sitter_externals_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}

void tree_sitter_externals_external_scanner_destroy(void *payload) {
    free(payload);
}
