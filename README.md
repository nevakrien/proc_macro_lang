# proc_macro_lang  (placeholder name)
a programing languge implemnted to be fully parsble as a rust proc_macro.
the languge is able to run stand alone or as part of rust and is intended for light scripting and macro writing for rust.

# Idea
the languge would be a fully extendble syntax similar to agda. we would allow new operators to be implemented and generally support unique parsing patterns. all tokens are fundementally rust tokens to allow for the full range of the languge within a proc macro.

there is no inherent diffrence between a token stream and function. a function call is simply a call to Eval(code,vars) under the hood. this is intended to allow for a more complete integration with rust syntax and powerful metaprograming using runtime macros.
as a result of this decision we must ship the entire parsing scheme at runtime as well (which we want to do anyway).

these types would be included because of rusts parsing:
1. token stream
2. paren group: \[({stream})\] 
3. operator: Union(#;!. .. ... etc)
4. word: Union(name,keyword)
5. literal: Union(str,char,int,float,... etc)

note that specifcly braces are NOT allowed as their own type. this is a convention from the rust parsing setup to force brackets to be balanced. it is not a bad idea to do this for our languge as well. if for some reason you need single braces using < > can work or any other delimiter.

types we add:
1. array\<T\> (dynamic array)
2. int (i64 allways) 
3. type (for our languge not rust)
4. parse rule: (like Add: a + b)
5. structs (could be defined with parse rules like Point x y = (x,y))
6. Union(A,B,...) (similar to rust enum and traits explictly keyed by types)
7. type alias (could be used with unions to construct enums similar to rust)

8. iter \<T\>
9. interace (some useful ones Into\<T\> Iter\<T\> Eq checked \<T\>  etc)
10. checked \<T\> (token stream representing a valid T)
...(more to come)

note that the stdlib would contain basic rust parsing functions like a way to parse an expressions generics etc.
these would probably be wrappers around syns basic functionality with some added wiggle room.

# Features (we want)
1. unique operator full prasing freedom, including multi dispatch.
2. flexible type system specifcly tailored for parsing
3. runtime macros and meta programing
4. full integration with rust 

# Limitations
this projet has a very large scope and thus we cant cover everything. the languge would likely be fairly slow as we are going to be using an ast interpter and not a VM.

we would also not cover parsing rules that break rust or non rust FFI.

syntax highlighting would likely be missing to avoid diffrent views between the embedded and non embedded feel.
package managment would be non existent since we dont want to clash with cargo.

tooling in general would likely be mostly reliant on rust.

# Plan
we would have the following modules

1. core : the core parsing module including most of the implemntation
2. lang_macro: proc macros for inlining the languge in rust
3. runtime: the runtime used both for stand alone and embedded code (for runtime macro evaluation).

the parser for the languge would ideally be self referntial.
once we have the way to check for a parse rule in the languge we should be able to use that for everything.
there is some tricky parts to this meta programing but it should fundementally be possible to do. 

the languge would not be loaded into any ast external to token streams. this is mostly for simplicity reasons but it also the only way to gurntee 100% free runtime time macros.

once we have the basic functionality down we could implement most of the standard libarary using this new tool for both assisting rust with macros and full stand alone implementations. the stdlib should contain a lot of the languge. for instance it is entirly possible that arithmetic could be part of the stdlib and not the core.