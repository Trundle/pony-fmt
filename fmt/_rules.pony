primitive _ParserRules
  fun rules(): String =>
    """
    // type
    DEF(provides);
      PRINT_INLINE();
      AST_NODE(TK_PROVIDES);
      RULE("provided type", type);
      DONE();

    // infix
    DEF(defaultarg);
      PRINT_INLINE();
      SCOPE();
      AST_NODE(TK_SEQ);
      RULE("default value", infix);
      DONE();

    // ID [COLON type] [ASSIGN defaultarg]
    DEF(param);
      AST_NODE(TK_PARAM);
      TOKEN("parameter name", TK_ID);
      SKIP("mandatory type declaration on parameter", TK_COLON);
      RULE("parameter type", type);
      IF(TK_ASSIGN, RULE("default value", defaultarg));
      DONE();

    // ELLIPSIS
    DEF(ellipsis);
      TOKEN(NULL, TK_ELLIPSIS);
      DONE();

    // TRUE | FALSE | INT | FLOAT | STRING
    DEF(literal);
      TOKEN("literal", TK_TRUE, TK_FALSE, TK_INT, TK_FLOAT, TK_STRING);
      DONE();

    // HASH postfix
    DEF(const_expr);
      PRINT_INLINE();
      TOKEN(NULL, TK_CONSTANT);
      RULE("formal argument value", postfix);
      DONE();

    // literal
    DEF(typeargliteral);
      AST_NODE(TK_VALUEFORMALARG);
      PRINT_INLINE();
      RULE("type argument", literal);
      DONE();

    // HASH postfix
    DEF(typeargconst);
      AST_NODE(TK_VALUEFORMALARG);
      PRINT_INLINE();
      RULE("formal argument value", const_expr);
      DONE();

    // type | typeargliteral | typeargconst
    DEF(typearg);
      RULE("type argument", type, typeargliteral, typeargconst);
      DONE();

    // ID [COLON type] [ASSIGN typearg]
    DEF(typeparam);
      AST_NODE(TK_TYPEPARAM);
      TOKEN("name", TK_ID);
      IF(TK_COLON, RULE("type constraint", type));
      IF(TK_ASSIGN, RULE("default type argument", typearg));
      DONE();

    // param {COMMA param}
    DEF(params);
      AST_NODE(TK_PARAMS);
      RULE("parameter", param, ellipsis);
      WHILE(TK_COMMA, RULE("parameter", param, ellipsis));
      DONE();

    // LSQUARE typeparam {COMMA typeparam} RSQUARE
    DEF(typeparams);
      AST_NODE(TK_TYPEPARAMS);
      SKIP(NULL, TK_LSQUARE, TK_LSQUARE_NEW);
      RULE("type parameter", typeparam);
      WHILE(TK_COMMA, RULE("type parameter", typeparam));
      TERMINATE("type parameters", TK_RSQUARE);
      DONE();

    // LSQUARE type {COMMA type} RSQUARE
    DEF(typeargs);
      AST_NODE(TK_TYPEARGS);
      SKIP(NULL, TK_LSQUARE);
      RULE("type argument", typearg);
      WHILE(TK_COMMA, RULE("type argument", typearg));
      TERMINATE("type arguments", TK_RSQUARE);
      DONE();

    // CAP
    DEF(cap);
      TOKEN("capability", TK_ISO, TK_TRN, TK_REF, TK_VAL, TK_BOX, TK_TAG);
      DONE();

    // GENCAP
    DEF(gencap);
      TOKEN("generic capability", TK_CAP_READ, TK_CAP_SEND, TK_CAP_SHARE,
        TK_CAP_ALIAS, TK_CAP_ANY);
      DONE();

    // AT
    DEF(bare);
      TOKEN("@", TK_AT);
      DONE();

    // ID [DOT ID] [typeargs] [CAP] [EPHEMERAL | ALIASED]
    DEF(nominal);
      AST_NODE(TK_NOMINAL);
      TOKEN("name", TK_ID);
      IFELSE(TK_DOT,
        TOKEN("name", TK_ID),
        AST_NODE(TK_NONE);
        REORDER(1, 0);
      );
      OPT RULE("type arguments", typeargs);
      OPT RULE("capability", cap, gencap);
      OPT TOKEN(NULL, TK_EPHEMERAL, TK_ALIASED);
      DONE();

    // PIPE type
    DEF(uniontype);
      INFIX_BUILD();
      AST_NODE(TK_UNIONTYPE);
      SKIP(NULL, TK_PIPE);
      RULE("type", type);
      DONE();

    // AMP type
    DEF(isecttype);
      INFIX_BUILD();
      TOKEN(NULL, TK_ISECTTYPE);
      RULE("type", type);
      DONE();

    // type {uniontype | isecttype}
    DEF(infixtype);
      RULE("type", type);
      SEQ("type", uniontype, isecttype);
      DONE();

    // COMMA infixtype {COMMA infixtype}
    DEF(tupletype);
      INFIX_BUILD();
      TOKEN(NULL, TK_COMMA);
      MAP_ID(TK_COMMA, TK_TUPLETYPE);
      RULE("type", infixtype);
      WHILE(TK_COMMA, RULE("type", infixtype));
      DONE();

    // (LPAREN | LPAREN_NEW) infixtype [tupletype] RPAREN
    DEF(groupedtype);
      PRINT_INLINE();
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      RULE("type", infixtype);
      OPT_NO_DFLT RULE("type", tupletype);
      SKIP(NULL, TK_RPAREN);
      SET_FLAG(AST_FLAG_IN_PARENS);
      DONE();

    // THIS
    DEF(thistype);
      PRINT_INLINE();
      AST_NODE(TK_THISTYPE);
      SKIP(NULL, TK_THIS);
      DONE();

    // type (COMMA type)*
    DEF(typelist);
      PRINT_INLINE();
      AST_NODE(TK_PARAMS);
      RULE("parameter type", type);
      WHILE(TK_COMMA, RULE("parameter type", type));
      DONE();

    // LBRACE [CAP] [ID] [typeparams] (LPAREN | LPAREN_NEW) [typelist] RPAREN
    // [COLON type] [QUESTION] RBRACE [CAP] [EPHEMERAL | ALIASED]
    DEF(lambdatype);
      AST_NODE(TK_LAMBDATYPE);
      SKIP(NULL, TK_LBRACE);
      OPT RULE("capability", cap);
      OPT TOKEN("function name", TK_ID);
      OPT RULE("type parameters", typeparams);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("parameters", typelist);
      SKIP(NULL, TK_RPAREN);
      IF(TK_COLON, RULE("return type", type));
      OPT TOKEN(NULL, TK_QUESTION);
      SKIP(NULL, TK_RBRACE);
      OPT RULE("capability", cap, gencap);
      OPT TOKEN(NULL, TK_EPHEMERAL, TK_ALIASED);
      DONE();

    // AT_LBRACE [CAP] [ID] [typeparams] (LPAREN | LPAREN_NEW) [typelist] RPAREN
    // [COLON type] [QUESTION] RBRACE [CAP] [EPHEMERAL | ALIASED]
    DEF(barelambdatype);
      AST_NODE(TK_BARELAMBDATYPE);
      SKIP(NULL, TK_AT_LBRACE);
      OPT RULE("capability", cap);
      OPT TOKEN("function name", TK_ID);
      OPT RULE("type parameters", typeparams);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("parameters", typelist);
      SKIP(NULL, TK_RPAREN);
      IF(TK_COLON, RULE("return type", type));
      OPT TOKEN(NULL, TK_QUESTION);
      SKIP(NULL, TK_RBRACE);
      OPT RULE("capability", cap, gencap);
      OPT TOKEN(NULL, TK_EPHEMERAL, TK_ALIASED);
      DONE();

    // (thistype | cap | typeexpr | nominal | lambdatype | barelambdatype)
    DEF(atomtype);
      RULE("type", thistype, cap, groupedtype, nominal, lambdatype, barelambdatype);
      DONE();

    // ARROW type
    DEF(viewpoint);
      PRINT_INLINE();
      INFIX_BUILD();
      TOKEN(NULL, TK_ARROW);
      RULE("viewpoint", type);
      DONE();

    // atomtype [viewpoint]
    DEF(type);
      RULE("type", atomtype);
      OPT_NO_DFLT RULE("viewpoint", viewpoint);
      DONE();

    // ID [$updatearg] ASSIGN rawseq
    DEF(namedarg);
      AST_NODE(TK_NAMEDARG);
      TOKEN("argument name", TK_ID);
      IFELSE(TK_TEST_UPDATEARG,
        MAP_ID(TK_NAMEDARG, TK_UPDATEARG),
        {}
      );
      SKIP(NULL, TK_ASSIGN);
      RULE("argument value", rawseq);
      DONE();

    // WHERE namedarg {COMMA namedarg}
    DEF(named);
      AST_NODE(TK_NAMEDARGS);
      SKIP(NULL, TK_WHERE);
      RULE("named argument", namedarg);
      WHILE(TK_COMMA, RULE("named argument", namedarg));
      DONE();

    // rawseq {COMMA rawseq}
    DEF(positional);
      AST_NODE(TK_POSITIONALARGS);
      RULE("argument", rawseq);
      WHILE(TK_COMMA, RULE("argument", rawseq));
      DONE();

    // '\' ID {COMMA ID} '\'
    DEF(annotations);
      PRINT_INLINE();
      TOKEN(NULL, TK_BACKSLASH);
      MAP_ID(TK_BACKSLASH, TK_ANNOTATION);
      TOKEN("annotation", TK_ID);
      WHILE(TK_COMMA, TOKEN("annotation", TK_ID));
      TERMINATE("annotations", TK_BACKSLASH);
      DONE();

    // OBJECT [annotations] [CAP] [IS type] members END
    DEF(object);
      PRINT_INLINE();
      TOKEN(NULL, TK_OBJECT);
      ANNOTATE(annotations);
      OPT RULE("capability", cap);
      IF(TK_IS, RULE("provided type", provides));
      RULE("object member", members);
      TERMINATE("object literal", TK_END);
      SET_CHILD_FLAG(0, AST_FLAG_PRESERVE); // Cap
      SET_CHILD_FLAG(1, AST_FLAG_PRESERVE); // Provides
      SET_CHILD_FLAG(2, AST_FLAG_PRESERVE); // Members
      DONE();

    // parampattern [COLON type] [ASSIGN defaultarg]
    DEF(lambdaparam);
      AST_NODE(TK_PARAM);
      TOKEN("parameter name", TK_ID);
      IF(TK_COLON, RULE("parameter type", type));
      IF(TK_ASSIGN, RULE("default value", defaultarg));
      DONE();

    // lambdaparam {COMMA lambdaparam}
    DEF(lambdaparams);
      AST_NODE(TK_PARAMS);
      RULE("parameter", lambdaparam);
      WHILE(TK_COMMA, RULE("parameter", lambdaparam));
      DONE();

    // ID [COLON type] [ASSIGN infix]
    DEF(lambdacapture);
      AST_NODE(TK_LAMBDACAPTURE);
      TOKEN("name", TK_ID);
      IF(TK_COLON, RULE("capture type", type));
      IF(TK_ASSIGN, RULE("capture value", infix));
      DONE();

    // (LPAREN | LPAREN_NEW) (lambdacapture | thisliteral)
    // {COMMA (lambdacapture | thisliteral)} RPAREN
    DEF(lambdacaptures);
      AST_NODE(TK_LAMBDACAPTURES);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      RULE("capture", lambdacapture, thisliteral);
      WHILE(TK_COMMA, RULE("capture", lambdacapture, thisliteral));
      SKIP(NULL, TK_RPAREN);
      DONE();

    // LBRACE [annotations] [CAP] [ID] [typeparams] (LPAREN | LPAREN_NEW)
    // [lambdaparams] RPAREN [lambdacaptures] [COLON type] [QUESTION] ARROW rawseq
    // RBRACE [CAP]
    DEF(lambda);
      PRINT_INLINE();
      AST_NODE(TK_LAMBDA);
      SKIP(NULL, TK_LBRACE);
      ANNOTATE(annotations);
      OPT RULE("receiver capability", cap);
      OPT TOKEN("function name", TK_ID);
      OPT RULE("type parameters", typeparams);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("parameters", lambdaparams);
      SKIP(NULL, TK_RPAREN);
      OPT RULE("captures", lambdacaptures);
      IF(TK_COLON, RULE("return type", type));
      OPT TOKEN(NULL, TK_QUESTION);
      SKIP(NULL, TK_DBLARROW);
      RULE("lambda body", rawseq);
      TERMINATE("lambda expression", TK_RBRACE);
      OPT RULE("reference capability", cap);
      SET_CHILD_FLAG(2, AST_FLAG_PRESERVE); // Type parameters
      SET_CHILD_FLAG(3, AST_FLAG_PRESERVE); // Parameters
      SET_CHILD_FLAG(5, AST_FLAG_PRESERVE); // Return type
      SET_CHILD_FLAG(7, AST_FLAG_PRESERVE); // Body
      DONE();

    // AT_LBRACE [annotations] [CAP] [ID] [typeparams] (LPAREN | LPAREN_NEW)
    // [lambdaparams] RPAREN [lambdacaptures] [COLON type] [QUESTION] ARROW rawseq
    // RBRACE [CAP]
    DEF(barelambda);
      PRINT_INLINE();
      AST_NODE(TK_BARELAMBDA);
      SKIP(NULL, TK_AT_LBRACE);
      ANNOTATE(annotations);
      OPT RULE("receiver capability", cap);
      OPT TOKEN("function name", TK_ID);
      OPT RULE("type parameters", typeparams);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("parameters", lambdaparams);
      SKIP(NULL, TK_RPAREN);
      OPT RULE("captures", lambdacaptures);
      IF(TK_COLON, RULE("return type", type));
      OPT TOKEN(NULL, TK_QUESTION);
      SKIP(NULL, TK_DBLARROW);
      RULE("lambda body", rawseq);
      TERMINATE("lambda expression", TK_RBRACE);
      OPT RULE("reference capability", cap);
      SET_CHILD_FLAG(2, AST_FLAG_PRESERVE); // Type parameters
      SET_CHILD_FLAG(3, AST_FLAG_PRESERVE); // Parameters
      SET_CHILD_FLAG(5, AST_FLAG_PRESERVE); // Return type
      SET_CHILD_FLAG(7, AST_FLAG_PRESERVE); // Body
      DONE();

    // AS type ':'
    DEF(arraytype);
      PRINT_INLINE();
      SKIP(NULL, TK_AS);
      RULE("type", type);
      SKIP(NULL, TK_COLON);
      DONE();

    // (LSQUARE | LSQUARE_NEW) [rawseq] RSQUARE
    DEF(array);
      PRINT_INLINE();
      AST_NODE(TK_ARRAY);
      SKIP(NULL, TK_LSQUARE, TK_LSQUARE_NEW);
      OPT RULE("element type", arraytype);
      OPT RULE("array elements", rawseq);
      TERMINATE("array literal", TK_RSQUARE);
      DONE();

    // LSQUARE_NEW rawseq [rawseq] RSQUARE
    DEF(nextarray);
      PRINT_INLINE();
      AST_NODE(TK_ARRAY);
      SKIP(NULL, TK_LSQUARE_NEW);
      OPT RULE("element type", arraytype);
      OPT RULE("array elements", rawseq);
      TERMINATE("array literal", TK_RSQUARE);
      DONE();

    // COMMA rawseq {COMMA rawseq}
    DEF(tuple);
      INFIX_BUILD();
      TOKEN(NULL, TK_COMMA);
      MAP_ID(TK_COMMA, TK_TUPLE);
      RULE("value", rawseq);
      WHILE(TK_COMMA, RULE("value", rawseq));
      DONE();

    // (LPAREN | LPAREN_NEW) rawseq [tuple] RPAREN
    DEF(groupedexpr);
      PRINT_INLINE();
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      RULE("value", rawseq);
      OPT_NO_DFLT RULE("value", tuple);
      SKIP(NULL, TK_RPAREN);
      SET_FLAG(AST_FLAG_IN_PARENS);
      DONE();

    // LPAREN_NEW rawseq [tuple] RPAREN
    DEF(nextgroupedexpr);
      PRINT_INLINE();
      SKIP(NULL, TK_LPAREN_NEW);
      RULE("value", rawseq);
      OPT_NO_DFLT RULE("value", tuple);
      SKIP(NULL, TK_RPAREN);
      SET_FLAG(AST_FLAG_IN_PARENS);
      DONE();

    // THIS
    DEF(thisliteral);
      TOKEN(NULL, TK_THIS);
      DONE();

    // ID
    DEF(ref);
      PRINT_INLINE();
      AST_NODE(TK_REFERENCE);
      TOKEN("name", TK_ID);
      DONE();

    // __LOC
    DEF(location);
      PRINT_INLINE();
      TOKEN(NULL, TK_LOCATION);
      DONE();

    // AT (ID | STRING) typeargs (LPAREN | LPAREN_NEW) [positional] RPAREN
    // [QUESTION]
    DEF(ffi);
      PRINT_INLINE();
      TOKEN(NULL, TK_AT);
      MAP_ID(TK_AT, TK_FFICALL);
      TOKEN("ffi name", TK_ID, TK_STRING);
      OPT RULE("return type", typeargs);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("ffi arguments", positional);
      OPT RULE("ffi arguments", named);
      TERMINATE("ffi arguments", TK_RPAREN);
      OPT TOKEN(NULL, TK_QUESTION);
      DONE();

    // atom
    // ref | this | literal | tuple | array | object | lambda | barelambda | ffi | cond | whileloop | forloop
    // location
    DEF(atom);
      RULE("value", ref, thisliteral, literal, groupedexpr, array, object, lambda,
        barelambda, ffi, location, cond, whileloop, forloop);
      DONE();

    // caseatom:
    // ref | this | literal | tuple | array | object | lambda | barelambda | ffi | whileloop | forloop
    // location
    DEF(caseatom);
      RULE("value", ref, thisliteral, literal, groupedexpr, array, object, lambda,
        barelambda, ffi, location, whileloop, forloop);
      DONE();

    // ref | this | literal | tuple | array | object | lambda | barelambda| ffi | cond | whileloop | forloop
    // location
    DEF(nextatom);
      RULE("value", ref, thisliteral, literal, nextgroupedexpr, nextarray, object,
        lambda, barelambda, ffi, location, cond, whileloop, forloop);
      DONE();

    // DOT ID
    DEF(dot);
      INFIX_BUILD();
      TOKEN(NULL, TK_DOT);
      TOKEN("member name", TK_ID);
      DONE();

    // TILDE ID
    DEF(tilde);
      INFIX_BUILD();
      TOKEN(NULL, TK_TILDE);
      TOKEN("method name", TK_ID);
      DONE();

    // CHAIN ID
    DEF(chain);
      INFIX_BUILD();
      TOKEN(NULL, TK_CHAIN);
      TOKEN("method name", TK_ID);
      DONE();

    // typeargs
    DEF(qualify);
      INFIX_BUILD();
      AST_NODE(TK_QUALIFY);
      RULE("type arguments", typeargs);
      DONE();

    // LPAREN [positional] [named] RPAREN [QUESTION]
    DEF(call);
      INFIX_BUILD();
      AST_NODE(TK_CALL);
      SKIP(NULL, TK_LPAREN);
      OPT RULE("argument", positional);
      OPT RULE("argument", named);
      TERMINATE("call arguments", TK_RPAREN);
      OPT TOKEN(NULL, TK_QUESTION);
      DONE();

    // atom {dot | tilde | chain | qualify | call}
    DEF(postfix);
      RULE("value", atom);
      SEQ("postfix expression", dot, tilde, chain, qualify, call);
      DONE();

    // atom {dot | tilde | chain | qualify | call}
    DEF(casepostfix);
      RULE("value", caseatom);
      SEQ("postfix expression", dot, tilde, chain, qualify, call);
      DONE();

    // atom {dot | tilde | chain | qualify | call}
    DEF(nextpostfix);
      RULE("value", nextatom);
      SEQ("postfix expression", dot, tilde, chain, qualify, call);
      DONE();

    // (VAR | LET | EMBED | $LET) ID [COLON type]
    DEF(local);
      PRINT_INLINE();
      TOKEN(NULL, TK_VAR, TK_LET, TK_EMBED, TK_MATCH_CAPTURE);
      TOKEN("variable name", TK_ID);
      IF(TK_COLON, RULE("variable type", type));
      DONE();

    // (NOT | AMP | MINUS | MINUS_TILDE | MINUS_NEW | MINUS_TILDE_NEW | DIGESTOF)
    // pattern
    DEF(prefix);
      PRINT_INLINE();
      TOKEN("prefix", TK_NOT, TK_ADDRESS, TK_MINUS, TK_MINUS_TILDE, TK_MINUS_NEW,
        TK_MINUS_TILDE_NEW, TK_DIGESTOF);
      MAP_ID(TK_MINUS, TK_UNARY_MINUS);
      MAP_ID(TK_MINUS_TILDE, TK_UNARY_MINUS_TILDE);
      MAP_ID(TK_MINUS_NEW, TK_UNARY_MINUS);
      MAP_ID(TK_MINUS_TILDE_NEW, TK_UNARY_MINUS_TILDE);
      RULE("expression", parampattern);
      DONE();

    // (NOT | AMP | MINUS | MINUS_TILDE | MINUS_NEW | MINUS_TILDE_NEW | DIGESTOF)
    // casepattern
    DEF(caseprefix);
      PRINT_INLINE();
      TOKEN("prefix", TK_NOT, TK_ADDRESS, TK_MINUS, TK_MINUS_TILDE, TK_MINUS_NEW,
        TK_MINUS_TILDE_NEW, TK_DIGESTOF);
      MAP_ID(TK_MINUS, TK_UNARY_MINUS);
      MAP_ID(TK_MINUS_TILDE, TK_UNARY_MINUS_TILDE);
      MAP_ID(TK_MINUS_NEW, TK_UNARY_MINUS);
      MAP_ID(TK_MINUS_TILDE_NEW, TK_UNARY_MINUS_TILDE);
      RULE("expression", caseparampattern);
      DONE();

    // (NOT | AMP | MINUS_NEW | MINUS_TILDE_NEW | DIGESTOF) pattern
    DEF(nextprefix);
      PRINT_INLINE();
      TOKEN("prefix", TK_NOT, TK_ADDRESS, TK_MINUS_NEW, TK_MINUS_TILDE_NEW,
        TK_DIGESTOF);
      MAP_ID(TK_MINUS_NEW, TK_UNARY_MINUS);
      MAP_ID(TK_MINUS_TILDE_NEW, TK_UNARY_MINUS_TILDE);
      RULE("expression", parampattern);
      DONE();

    // (prefix | postfix)
    DEF(parampattern);
      RULE("pattern", prefix, postfix);
      DONE();

    // (caseprefix | casepostfix)
    DEF(caseparampattern);
      RULE("pattern", caseprefix, casepostfix);
      DONE();

    // (prefix | postfix)
    DEF(nextparampattern);
      RULE("pattern", nextprefix, nextpostfix);
      DONE();

    // (local | prefix | postfix)
    DEF(pattern);
      RULE("pattern", local, parampattern);
      DONE();

    // (local | prefix | postfix)
    DEF(casepattern);
      RULE("pattern", local, caseparampattern);
      DONE();

    // (local | prefix | postfix)
    DEF(nextpattern);
      RULE("pattern", local, nextparampattern);
      DONE();

    // (LPAREN | LPAREN_NEW) idseq {COMMA idseq} RPAREN
    DEF(idseqmulti);
      PRINT_INLINE();
      AST_NODE(TK_TUPLE);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      RULE("variable name", idseq_in_seq);
      WHILE(TK_COMMA, RULE("variable name", idseq_in_seq));
      SKIP(NULL, TK_RPAREN);
      DONE();

    // ID
    DEF(idseqsingle);
      PRINT_INLINE();
      AST_NODE(TK_LET);
      TOKEN("variable name", TK_ID);
      AST_NODE(TK_NONE);  // Type
      DONE();

    // idseq
    DEF(idseq_in_seq);
      AST_NODE(TK_SEQ);
      RULE("variable name", idseqsingle, idseqmulti);
      DONE();

    // ID | (LPAREN | LPAREN_NEW) idseq {COMMA idseq} RPAREN
    DEF(idseq);
      RULE("variable name", idseqsingle, idseqmulti);
      DONE();

    // ELSE annotatedseq
    DEF(elseclause);
      PRINT_INLINE();
      SKIP(NULL, TK_ELSE);
      RULE("else value", annotatedseq);
      DONE();

    // ELSEIF [annotations] rawseq THEN seq [elseif | (ELSE seq)]
    DEF(elseif);
      AST_NODE(TK_IF);
      SCOPE();
      SKIP(NULL, TK_ELSEIF);
      ANNOTATE(annotations);
      RULE("condition expression", rawseq);
      SKIP(NULL, TK_THEN);
      RULE("then value", seq);
      OPT RULE("else clause", elseif, elseclause);
      DONE();

    // IF [annotations] rawseq THEN seq [elseif | elseclause] END
    DEF(cond);
      PRINT_INLINE();
      TOKEN(NULL, TK_IF);
      ANNOTATE(annotations);
      SCOPE();
      RULE("condition expression", rawseq);
      SKIP(NULL, TK_THEN);
      RULE("then value", seq);
      OPT RULE("else clause", elseif, elseclause);
      TERMINATE("if expression", TK_END);
      DONE();

    // ELSEIF [annotations] infix [$EXTRA infix] THEN seq [elseifdef | elseclause]
    DEF(elseifdef);
      AST_NODE(TK_IFDEF);
      SCOPE();
      SKIP(NULL, TK_ELSEIF);
      ANNOTATE(annotations);
      RULE("condition expression", infix);
      IF(TK_TEST_EXTRA, RULE("else condition", infix));
      SKIP(NULL, TK_THEN);
      RULE("then value", seq);
      OPT RULE("else clause", elseifdef, elseclause);
      // Order should be:
      // condition then_clause else_clause else_condition
      REORDER(0, 2, 3, 1);
      DONE();

    // IFDEF [annotations] infix [$EXTRA infix] THEN seq [elseifdef | elseclause]
    // END
    DEF(ifdef);
      PRINT_INLINE();
      TOKEN(NULL, TK_IFDEF);
      ANNOTATE(annotations);
      SCOPE();
      RULE("condition expression", infix);
      IF(TK_TEST_EXTRA, RULE("else condition", infix));
      SKIP(NULL, TK_THEN);
      RULE("then value", seq);
      OPT RULE("else clause", elseifdef, elseclause);
      TERMINATE("ifdef expression", TK_END);
      // Order should be:
      // condition then_clause else_clause else_condition
      REORDER(0, 2, 3, 1);
      DONE();

    // type <: type THEN seq
    DEF(iftype);
      AST_NODE(TK_IFTYPE);
      SCOPE();
      RULE("type", type);
      SKIP(NULL, TK_SUBTYPE);
      RULE("type", type);
      SKIP(NULL, TK_THEN);
      RULE("then value", seq);
      AST_NODE(TK_NONE);
      DONE();

    // ELSEIF [annotations] iftype [elseiftype | (ELSE seq)]
    DEF(elseiftype);
      AST_NODE(TK_IFTYPE_SET);
      SKIP(NULL, TK_ELSEIF);
      ANNOTATE(annotations);
      SCOPE();
      RULE("iftype clause", iftype);
      OPT RULE("else clause", elseiftype, elseclause);
      DONE();

    // IFTYPE_SET [annotations] iftype [elseiftype | (ELSE seq)] END
    DEF(iftypeset);
      PRINT_INLINE();
      TOKEN(NULL, TK_IFTYPE_SET);
      SCOPE();
      ANNOTATE(annotations);
      RULE("iftype clause", iftype);
      OPT RULE("else clause", elseiftype, elseclause);
      TERMINATE("iftype expression", TK_END);
      DONE();

    // PIPE [annotations] [infix] [IF rawseq] [ARROW rawseq]
    DEF(caseexpr);
      AST_NODE(TK_CASE);
      SCOPE();
      SKIP(NULL, TK_PIPE);
      ANNOTATE(annotations);
      OPT RULE("case pattern", casepattern);
      IF(TK_IF, RULE("guard expression", rawseq));
      IF(TK_DBLARROW, RULE("case body", rawseq));
      DONE();

    // {caseexpr}
    DEF(cases);
      PRINT_INLINE();
      AST_NODE(TK_CASES);
      SCOPE();  // Cases a scope to simplify branch consolidation.
      SEQ("cases", caseexpr);
      DONE();

    // MATCH [annotations] rawseq cases [ELSE annotatedseq] END
    DEF(match);
      PRINT_INLINE();
      TOKEN(NULL, TK_MATCH);
      ANNOTATE(annotations);
      SCOPE();
      RULE("match expression", rawseq);
      RULE("cases", cases);
      IF(TK_ELSE, RULE("else clause", annotatedseq));
      TERMINATE("match expression", TK_END);
      DONE();

    // WHILE [annotations] rawseq DO seq [ELSE annotatedseq] END
    DEF(whileloop);
      PRINT_INLINE();
      TOKEN(NULL, TK_WHILE);
      ANNOTATE(annotations);
      SCOPE();
      RULE("condition expression", rawseq);
      SKIP(NULL, TK_DO);
      RULE("while body", seq);
      IF(TK_ELSE, RULE("else clause", annotatedseq));
      TERMINATE("while loop", TK_END);
      DONE();

    // REPEAT [annotations] seq UNTIL annotatedrawseq [ELSE annotatedseq] END
    DEF(repeat);
      PRINT_INLINE();
      TOKEN(NULL, TK_REPEAT);
      ANNOTATE(annotations);
      SCOPE();
      RULE("repeat body", seq);
      SKIP(NULL, TK_UNTIL);
      RULE("condition expression", annotatedseq);
      IF(TK_ELSE, RULE("else clause", annotatedseq));
      TERMINATE("repeat loop", TK_END);
      DONE();

    // FOR [annotations] idseq IN rawseq DO rawseq [ELSE annotatedseq] END
    // =>
    // (SEQ
    //   (ASSIGN (LET $1) iterator)
    //   (WHILE $1.has_next()
    //     (SEQ (ASSIGN idseq $1.next()) body) else))
    // The body is not a scope since the sugar wraps it in a seq for us.
    DEF(forloop);
      PRINT_INLINE();
      TOKEN(NULL, TK_FOR);
      ANNOTATE(annotations);
      RULE("iterator name", idseq);
      SKIP(NULL, TK_IN);
      RULE("iterator", rawseq);
      SKIP(NULL, TK_DO);
      RULE("for body", rawseq);
      IF(TK_ELSE, RULE("else clause", annotatedseq));
      TERMINATE("for loop", TK_END);
      DONE();

    // idseq = rawseq
    DEF(withelem);
      AST_NODE(TK_SEQ);
      RULE("with name", idseq);
      SKIP(NULL, TK_ASSIGN);
      RULE("initialiser", rawseq);
      DONE();

    // withelem {COMMA withelem}
    DEF(withexpr);
      PRINT_INLINE();
      AST_NODE(TK_SEQ);
      RULE("with expression", withelem);
      WHILE(TK_COMMA, RULE("with expression", withelem));
      DONE();

    // WITH [annotations] withexpr DO rawseq [ELSE annotatedrawseq] END
    // =>
    // (SEQ
    //   (ASSIGN (LET $1 initialiser))*
    //   (TRY_NO_CHECK
    //     (SEQ (ASSIGN idseq $1)* body)
    //     (SEQ (ASSIGN idseq $1)* else)
    //     (SEQ $1.dispose()*)))
    // The body and else clause aren't scopes since the sugar wraps them in seqs
    // for us.
    DEF(with);
      PRINT_INLINE();
      TOKEN(NULL, TK_WITH);
      ANNOTATE(annotations);
      RULE("with expression", withexpr);
      SKIP(NULL, TK_DO);
      RULE("with body", rawseq);
      IF(TK_ELSE, RULE("else clause", annotatedrawseq));
      TERMINATE("with expression", TK_END);
      DONE();

    // TRY [annotations] seq [ELSE annotatedseq] [THEN annotatedseq] END
    DEF(try_block);
      PRINT_INLINE();
      TOKEN(NULL, TK_TRY);
      ANNOTATE(annotations);
      RULE("try body", seq);
      IF(TK_ELSE, RULE("try else body", annotatedseq));
      IF(TK_THEN, RULE("try then body", annotatedseq));
      TERMINATE("try expression", TK_END);
      DONE();

    // $TRY_NO_CHECK [annotations] seq [ELSE annotatedseq] [THEN annotatedseq] END
    DEF(test_try_block);
      PRINT_INLINE();
      TOKEN(NULL, TK_TEST_TRY_NO_CHECK);
      ANNOTATE(annotations);
      MAP_ID(TK_TEST_TRY_NO_CHECK, TK_TRY_NO_CHECK);
      RULE("try body", seq);
      IF(TK_ELSE, RULE("try else body", annotatedseq));
      IF(TK_THEN, RULE("try then body", annotatedseq));
      TERMINATE("try expression", TK_END);
      DONE();

    // RECOVER [annotations] [CAP] seq END
    DEF(recover);
      PRINT_INLINE();
      TOKEN(NULL, TK_RECOVER);
      ANNOTATE(annotations);
      OPT RULE("capability", cap);
      RULE("recover body", seq);
      TERMINATE("recover expression", TK_END);
      DONE();

    // $ALIASED
    DEF(test_aliased);
      PRINT_INLINE();
      TOKEN(NULL, TK_TEST_ALIASED);
      MAP_ID(TK_TEST_ALIASED, TK_ALIASED);
      DONE();

    // CONSUME [cap | test_aliased] term
    DEF(consume);
      PRINT_INLINE();
      TOKEN("consume", TK_CONSUME);
      OPT RULE("capability", cap, test_aliased);
      RULE("expression", term);
      DONE();

    // $IFDEFNOT term
    DEF(test_prefix);
      PRINT_INLINE();
      TOKEN(NULL, TK_IFDEFNOT);
      RULE("expression", term);
      DONE();

    // $NOSEQ '(' infix ')'
    // For testing only, thrown out by syntax pass
    DEF(test_noseq);
      PRINT_INLINE();
      SKIP(NULL, TK_TEST_NO_SEQ);
      SKIP(NULL, TK_LPAREN);
      RULE("sequence", infix);
      SKIP(NULL, TK_RPAREN);
      DONE();

    // $SCOPE '(' rawseq ')'
    // For testing only, thrown out by syntax pass
    DEF(test_seq_scope);
      PRINT_INLINE();
      SKIP(NULL, TK_TEST_SEQ_SCOPE);
      SKIP(NULL, TK_LPAREN);
      RULE("sequence", rawseq);
      SKIP(NULL, TK_RPAREN);
      SCOPE();
      DONE();

    // $IFDEFFLAG id
    // For testing only, thrown out by syntax pass
    DEF(test_ifdef_flag);
      PRINT_INLINE();
      TOKEN(NULL, TK_IFDEFFLAG);
      TOKEN(NULL, TK_ID);
      DONE();

    // cond | ifdef | iftypeset | match | whileloop | repeat | forloop | with | try |
    // recover | consume | pattern | const_expr | test_<various>
    DEF(term);
      RULE("value", cond, ifdef, iftypeset, match, whileloop, repeat, forloop, with,
        try_block, recover, consume, pattern, const_expr, test_noseq,
        test_seq_scope, test_try_block, test_ifdef_flag, test_prefix);
      DONE();

    // cond | ifdef | iftypeset | match | whileloop | repeat | forloop | with | try |
    // recover | consume | pattern | const_expr | test_<various>
    DEF(nextterm);
      RULE("value", cond, ifdef, iftypeset, match, whileloop, repeat, forloop, with,
        try_block, recover, consume, nextpattern, const_expr, test_noseq,
        test_seq_scope, test_try_block, test_ifdef_flag, test_prefix);
      DONE();

    // AS type
    // For tuple types, use multiple matches.
    // (AS expr type) =>
    // (MATCH expr
    //   (CASES
    //     (CASE
    //       (LET $1 type)
    //       NONE
    //       (SEQ (CONSUME ALIASED $1))))
    //   (SEQ ERROR))
    DEF(asop);
      PRINT_INLINE();
      INFIX_BUILD();
      TOKEN("as", TK_AS);
      RULE("type", type);
      DONE();

    // BINOP [QUESTION] term
    DEF(binop);
      INFIX_BUILD();
      TOKEN("binary operator",
        TK_AND, TK_OR, TK_XOR,
        TK_PLUS, TK_MINUS, TK_MULTIPLY, TK_DIVIDE, TK_REM, TK_MOD,
        TK_PLUS_TILDE, TK_MINUS_TILDE, TK_MULTIPLY_TILDE, TK_DIVIDE_TILDE,
        TK_REM_TILDE, TK_MOD_TILDE,
        TK_LSHIFT, TK_RSHIFT, TK_LSHIFT_TILDE, TK_RSHIFT_TILDE,
        TK_EQ, TK_NE, TK_LT, TK_LE, TK_GE, TK_GT,
        TK_EQ_TILDE, TK_NE_TILDE, TK_LT_TILDE, TK_LE_TILDE, TK_GE_TILDE, TK_GT_TILDE
        );
      OPT TOKEN(NULL, TK_QUESTION);
      RULE("value", term);
      REORDER(1, 0);
      DONE();

    // [IS | ISNT] term
    DEF(isop);
      INFIX_BUILD();
      TOKEN("binary operator", TK_IS, TK_ISNT);
      RULE("value", term);
      DONE();

    // TEST_BINOP term
    // For testing only, thrown out by syntax pass
    DEF(test_binop);
      INFIX_BUILD();
      TOKEN("binary operator", TK_IFDEFAND, TK_IFDEFOR);
      RULE("value", term);
      DONE();

    // term {binop | isop | asop}
    DEF(infix);
      RULE("value", term);
      SEQ("value", binop, isop, asop, test_binop);
      DONE();

    // term {binop | isop | asop}
    DEF(nextinfix);
      RULE("value", nextterm);
      SEQ("value", binop, isop, asop, test_binop);
      DONE();

    // ASSIGNOP assignment
    DEF(assignop);
      PRINT_INLINE();
      INFIX_BUILD();
      TOKEN("assign operator", TK_ASSIGN);
      RULE("assign rhs", assignment);
      DONE();

    // term [assignop]
    DEF(assignment);
      RULE("value", infix);
      OPT_NO_DFLT RULE("value", assignop);
      DONE();

    // term [assignop]
    DEF(nextassignment);
      RULE("value", nextinfix);
      OPT_NO_DFLT RULE("value", assignop);
      DONE();

    // RETURN | BREAK | CONTINUE | ERROR | COMPILE_INTRINSIC | COMPILE_ERROR
    DEF(jump);
      TOKEN("statement", TK_RETURN, TK_BREAK, TK_CONTINUE, TK_ERROR,
        TK_COMPILE_INTRINSIC, TK_COMPILE_ERROR);
      OPT RULE("return value", rawseq);
      DONE();

    // SEMI
    DEF(semi);
      IFELSE(TK_NEWLINE, NEXT_FLAGS(AST_FLAG_BAD_SEMI), NEXT_FLAGS(0));
      TOKEN(NULL, TK_SEMI);
      IF(TK_NEWLINE, SET_FLAG(AST_FLAG_BAD_SEMI));
      DONE();

    // semi (exprseq | jump)
    DEF(semiexpr);
      AST_NODE(TK_FLATTEN);
      RULE("semicolon", semi);
      RULE("value", exprseq, jump);
      DONE();

    // nextexprseq | jump
    DEF(nosemi);
      IFELSE(TK_NEWLINE, NEXT_FLAGS(0), NEXT_FLAGS(AST_FLAG_MISSING_SEMI));
      RULE("value", nextexprseq, jump);
      DONE();

    // nextassignment (semiexpr | nosemi)
    DEF(nextexprseq);
      AST_NODE(TK_FLATTEN);
      RULE("value", nextassignment);
      OPT_NO_DFLT RULE("value", semiexpr, nosemi);
      NEXT_FLAGS(0);
      DONE();

    // assignment (semiexpr | nosemi)
    DEF(exprseq);
      AST_NODE(TK_FLATTEN);
      RULE("value", assignment);
      OPT_NO_DFLT RULE("value", semiexpr, nosemi);
      NEXT_FLAGS(0);
      DONE();

    // (exprseq | jump)
    DEF(rawseq);
      AST_NODE(TK_SEQ);
      RULE("value", exprseq, jump);
      DONE();

    // rawseq
    DEF(seq);
      RULE("value", rawseq);
      SCOPE();
      DONE();

    // [annotations] (exprseq | jump)
    DEF(annotatedrawseq);
      AST_NODE(TK_SEQ);
      ANNOTATE(annotations);
      RULE("value", exprseq, jump);
      DONE();

    // annotatedrawseq
    DEF(annotatedseq);
      RULE("value", annotatedrawseq);
      SCOPE();
      DONE();

    // (FUN | BE | NEW) [annotations] [CAP | AT] ID [typeparams]
    // (LPAREN | LPAREN_NEW) [params] RPAREN [COLON type] [QUESTION] [ARROW rawseq]
    DEF(method);
      TOKEN(NULL, TK_FUN, TK_BE, TK_NEW);
      ANNOTATE(annotations);
      SCOPE();
      OPT RULE("capability", cap, bare);
      TOKEN("method name", TK_ID);
      OPT RULE("type parameters", typeparams);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("parameters", params);
      SKIP(NULL, TK_RPAREN);
      IF(TK_COLON, RULE("return type", type));
      OPT TOKEN(NULL, TK_QUESTION);
      OPT TOKEN(NULL, TK_STRING);
      IF(TK_DBLARROW, RULE("method body", rawseq));
      // Order should be:
      // cap id type_params params return_type error body docstring
      REORDER(0, 1, 2, 3, 4, 5, 7, 6);
      DONE();

    // (VAR | LET | EMBED) ID [COLON type] [ASSIGN infix]
    DEF(field);
      TOKEN(NULL, TK_VAR, TK_LET, TK_EMBED);
      MAP_ID(TK_VAR, TK_FVAR);
      MAP_ID(TK_LET, TK_FLET);
      TOKEN("field name", TK_ID);
      SKIP("mandatory type declaration on field", TK_COLON);
      RULE("field type", type);
      IF(TK_ASSIGN, RULE("field value", infix));
      OPT TOKEN("docstring", TK_STRING);
      DONE();

    // {field} {method}
    DEF(members);
      AST_NODE(TK_MEMBERS);
      SEQ("field", field);
      SEQ("method", method);
      DONE();

    // (TYPE | INTERFACE | TRAIT | PRIMITIVE | STRUCT | CLASS | ACTOR) [annotations]
    // [AT] ID [typeparams] [CAP] [IS type] [STRING] members
    DEF(class_def);
      RESTART(TK_TYPE, TK_INTERFACE, TK_TRAIT, TK_PRIMITIVE, TK_STRUCT, TK_CLASS,
        TK_ACTOR);
      TOKEN("entity", TK_TYPE, TK_INTERFACE, TK_TRAIT, TK_PRIMITIVE, TK_STRUCT,
        TK_CLASS, TK_ACTOR);
      ANNOTATE(annotations);
      SCOPE();
      OPT TOKEN(NULL, TK_AT);
      OPT RULE("capability", cap);
      TOKEN("name", TK_ID);
      OPT RULE("type parameters", typeparams);
      IF(TK_IS, RULE("provided type", provides));
      OPT TOKEN("docstring", TK_STRING);
      RULE("members", members);
      // Order should be:
      // id type_params cap provides members c_api docstring
      REORDER(2, 3, 1, 4, 6, 0, 5);
      DONE();

    // STRING
    DEF(use_uri);
      PRINT_INLINE();
      TOKEN(NULL, TK_STRING);
      DONE();

    // AT (ID | STRING) typeparams (LPAREN | LPAREN_NEW) [params] RPAREN [QUESTION]
    DEF(use_ffi);
      TOKEN(NULL, TK_AT);
      MAP_ID(TK_AT, TK_FFIDECL);
      SCOPE();
      TOKEN("ffi name", TK_ID, TK_STRING);
      RULE("return type", typeargs);
      SKIP(NULL, TK_LPAREN, TK_LPAREN_NEW);
      OPT RULE("ffi parameters", params);
      AST_NODE(TK_NONE);  // Named parameters
      SKIP(NULL, TK_RPAREN);
      OPT TOKEN(NULL, TK_QUESTION);
      DONE();

    // ID ASSIGN
    DEF(use_name);
      PRINT_INLINE();
      TOKEN(NULL, TK_ID);
      SKIP(NULL, TK_ASSIGN);
      DONE();

    // USE [ID ASSIGN] (STRING | USE_FFI) [IF infix]
    DEF(use);
      RESTART(TK_USE, TK_TYPE, TK_INTERFACE, TK_TRAIT, TK_PRIMITIVE, TK_STRUCT,
        TK_CLASS, TK_ACTOR);
      TOKEN(NULL, TK_USE);
      OPT RULE("name", use_name);
      RULE("specifier", use_uri, use_ffi);
      IF(TK_IF, RULE("use condition", infix));
      DONE();

    // {use} {class}
    DEF(module);
      AST_NODE(TK_MODULE);
      SCOPE();
      OPT_NO_DFLT TOKEN("package docstring", TK_STRING);
      SEQ("use command", use);
      SEQ("type, interface, trait, primitive, class or actor definition",
        class_def);
      SKIP("type, interface, trait, primitive, class, actor, member or method",
        TK_EOF);
      DONE();
    """

