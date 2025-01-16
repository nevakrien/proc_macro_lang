use proc_macro2::{TokenStream, TokenTree,token_stream::IntoIter};


#[derive(Debug)]
pub enum Pattern{
	Literal(TokenStream)
}


/// Compares an expected `TokenStream` to an actual `IntoIter` consuming from the iter in the process.
/// Returns `Ok(())` if they match exactly, or `Err((expected, actual))` if they don't.
/// Note that on fail the iter passed in could overconsume parthesised expressions.
pub fn parse_exact_match(expected: &TokenStream, actual_iter: &mut IntoIter) -> Result<(), (TokenTree, TokenTree)> {
    for expected_token in expected.clone().into_iter() {
        match actual_iter.next() {
            Some(actual_token) => {
                let matches = match (&expected_token, &actual_token) {
                    (TokenTree::Group(expected_group), TokenTree::Group(actual_group)) => {
                        if expected_group.delimiter() == actual_group.delimiter() {
                            //this can overconsume on fail because we are making a new stream
                            //however on sucesses it will allways consume exacly enough
                            parse_exact_match(&expected_group.stream(), &mut actual_group.stream().into_iter())?;
                            true
                        } else {
                            false
                        }
                    }
                    (TokenTree::Literal(expected_lit), TokenTree::Literal(actual_lit)) => {
                        expected_lit.to_string() == actual_lit.to_string()
                    }
                    (TokenTree::Ident(expected_ident), TokenTree::Ident(actual_ident)) => {
                        expected_ident == actual_ident
                    }
                    (TokenTree::Punct(expected_punct), TokenTree::Punct(actual_punct)) => {
                        expected_punct.as_char() == actual_punct.as_char()
                    }
                    _ => false,
                };

                if !matches {
                    return Err((expected_token, actual_token));
                }
            }
            None => {
                return Err((
                    expected_token,
                    TokenTree::Literal(proc_macro2::Literal::string("<EOF>")),
                ));
            }
        }
    }

    Ok(())
}


#[test]
fn test_parse_exact_match() {
	use proc_macro2::Delimiter;

    // Test 1: Simple token match
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a b c;".parse::<TokenStream>().unwrap().into_iter();
        assert!(parse_exact_match(&expected, &mut actual_iter).is_ok());
    }

    // Test 2: Extra spaces should not affect matching
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a     b c ;".parse::<TokenStream>().unwrap().into_iter();
        assert!(parse_exact_match(&expected, &mut actual_iter).is_ok());
    }

    // Test 3: Comments should not affect matching
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a /* comment */ b // inline comment\n c ;"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        assert!(parse_exact_match(&expected, &mut actual_iter).is_ok());
    }

    // Test 4: Mismatched tokens
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a b d;".parse::<TokenStream>().unwrap().into_iter();
        let err = parse_exact_match(&expected, &mut actual_iter).unwrap_err();
        match err {
            (TokenTree::Ident(left), TokenTree::Ident(right)) => {
                assert_eq!(left, "c");
                assert_eq!(right, "d");
            }
            _ => panic!("Expected a mismatch of identifiers, got {:?}", err),
        }
    }

    // Test 5: Nested groups with exact match
    {
        let expected: TokenStream = "a (b {c}) d;".parse().unwrap();
        let mut actual_iter = "a (b {c}) d;".parse::<TokenStream>().unwrap().into_iter();
        assert!(parse_exact_match(&expected, &mut actual_iter).is_ok());
    }

    // Test 6: Nested groups with mismatched delimiters
    {
        let expected: TokenStream = "a (b {c}) d;".parse().unwrap();
        let mut actual_iter = "a [b {c}] d;".parse::<TokenStream>().unwrap().into_iter();
        let err = parse_exact_match(&expected, &mut actual_iter).unwrap_err();
        match err {
            (TokenTree::Group(left), TokenTree::Group(right)) => {
                assert_eq!(left.delimiter(), Delimiter::Parenthesis);
                assert_eq!(right.delimiter(), Delimiter::Bracket);
            }
            _ => panic!("Expected a mismatch of group delimiters, got {:?}", err),
        }
    }

    // Test 7: Nested groups with mismatched inner content
    {
        let expected: TokenStream = "a (b {c}) d;".parse().unwrap();
        let mut actual_iter = "a (b {x}) d;".parse::<TokenStream>().unwrap().into_iter();
        let _err = parse_exact_match(&expected, &mut actual_iter).unwrap_err();
    }

    // Test 8: Complex non-Rust syntax
    {
        let expected: TokenStream = "custom_function(arg1,arg2,arg3);".parse().unwrap();
        let mut actual_iter = "custom_function(arg1, arg2 /* comment */, arg3);"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        assert!(parse_exact_match(&expected, &mut actual_iter).is_ok());
    }

    // Test 9: Different spacing and comments within nested groups
    {
        let expected: TokenStream = "outer(inner1{inner2[data]});".parse().unwrap();
        let mut actual_iter = "outer ( inner1 /* inline */ { inner2 // comment\n [ data ] } ) ;"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        assert!(parse_exact_match(&expected, &mut actual_iter).is_ok());
    }

    // Test 10: Unexpected EOF in actual tokens
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a b".parse::<TokenStream>().unwrap().into_iter();
        let err = parse_exact_match(&expected, &mut actual_iter).unwrap_err();
        match err {
            (TokenTree::Ident(left), TokenTree::Literal(right)) => {
                assert_eq!(left, "c");
                assert_eq!(right.to_string(), "\"<EOF>\"");
            }
            _ => panic!("Expected an EOF mismatch, got {:?}", err),
        }
    }

    // Test 11: Extra tokens in actual tokens
    {
        let expected: TokenStream = "a b".parse().unwrap();
        let mut actual_iter = "a b c;".parse::<TokenStream>().unwrap().into_iter();
			parse_exact_match(&expected, &mut actual_iter).unwrap();
    }
}
