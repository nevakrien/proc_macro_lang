# proc_macro_lang  (placeholder name)
a programing languge implemnted to be fully parsble as a rust proc_macro.
the languge is able to run stand alone or as part of rust and is intended for light scripting and macro writing for rust.


currently we have a backend for basic pakerat parsing.

# Idea
the languge would be a fully extendble syntax similar to agda. we would allow new operators to be implemented and generally support unique parsing patterns. all tokens are fundementally rust tokens to allow for the full range of the languge within a proc macro.


parsing should not be allowed to go into an infinite loop. or at least it should be very hard/impossible to do so by acident without writing a loop explictly.
we do want something more powerful than a regex since recursive patterns do exist. but we don't want to allow a parser to not hault.


for this reason we enforce that syntax parsers would be pure so that we can detect infinite loops via caching. for T diffrent pure functions there are at most O(T\*N) diffrent function calls. which means that we must either detect an error/infinite loop or terminate within linear time.


to simplify the languge calling a forward declared parser is inherently related to its type.
you may have a pattern like so 

\[++\] a:int => (a+1):int

usage of this pattern is open to arbitrary expantion by the caller. for instance if the caller defines the rule

\[A\] => 1:int

then this implies that

\[++\] \[A\] => 2:int


the order in which rules apply is controled by a single float that ranks the priority of every rule. rules apply from highest priority to lowest stoping at the first rule that fits. This parsing framework is called PEG.


PEG has some shorcomings for instance the rule :

a:int \[+\] b:int => (a+b):int

will never parse sucessfully.
if it is called first then "+" will call "int" which will imidiatly call "+".
if it is called second then it will allways be parsed like so:

a:int + \[+ b:int\]

this can be fixed by making a type alias for int. if "+" calls "terminal int" which would not call "+" then everything is fine. so a working solution would be something like


a:term_int \[+\] b:int => ((int)a+b):int

this solution is right recursive but crucially it is not left recursive.



# Types

### rust ast:

1. token_tree (union of the types below)
2. paren group: \[({stream})\] 
3. operator: Union(#;!. .. ... etc)
4. word: Union(name,keyword)
5. literal: Union(str,char,int,float,... etc)

note that specifcly braces are NOT allowed as their own type. this is a convention from the rust parsing setup to force brackets to be balanced. it is not a bad idea to do this for our languge as well. if for some reason you need single braces using < > can work or any other delimiter.

### types we added:

1. int (i64 allways) 
2. none
3. externals (holds any rust type)
4. alias\<T\> (eacg akuas us unique)
5. array\<T\> (dynamic array)
6. parser\<T\> (used to allow function calls)
7. Union(A,B,...) (similar to rust enum and traits explictly keyed by types)
8. structs

note that the stdlib would contain basic rust parsing functions like a way to parse an expressions generics etc.
these would probably be wrappers around syns basic functionality with some added wiggle room.

# Features (we want)
1. unique operator full prasing freedom, including multi dispatch.
2. flexible type system specifcly tailored for parsing
3. runtime macros and meta programing
4. full integration with rust 
5. good rust parsing (in the stdlib)

# Limitations
this projet has a very large scope and thus we cant cover everything. the languge would likely be fairly slow as we are going to be using an ast interpter and not a VM. Further more everything uses Rc everywhere which is probably fairly expensive for no reason.

we would also not cover parsing rules that break rust or non rust FFI.

syntax highlighting would likely be missing to avoid diffrent views between the embedded and non embedded feel.
package managment would be non existent since we dont want to clash with cargo.

tooling in general would likely be mostly reliant on rust.

# Rc memory
so for Rced memory its EXTREMLY important to avoid cycles. one way to do so is imutability.
once an Rc was created in a struct it is never changed for any reason. this enforces the Rc refrence graph to be a directed asyclic graph.

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