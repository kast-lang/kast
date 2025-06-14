module:

const push = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "list.push"
        | target.name == "javascript" => (list_ref, value) => (
            native "$(list_ref).get().push($(value))"
        )
    } :: (&list[T], T) -> ()
};
const set = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "list.set"
        | target.name == "javascript" => (list_ref, index, value) => (
            native "$(list_ref).get()[$(index)]=$(value)"
        )
    } :: (&list[T], int32, T) -> ()
};
const length = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "list.length"
        | target.name == "javascript" => list_ref => (
            native "$(list_ref).get().length"
        )
    } :: &list[T] -> int32
};
const iter = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "list.iter"
        | target.name == "javascript" => list_ref => (
            let handle = (current generator_handler[&T]).handle;
            native "(()=>{let list=$(list_ref).get();for(let i=0;i<list.length;i++)$(handle)(ctx,{get:()=>list[i],set:(v)=>list[i]=v})})()"
        )
    } :: &list[T] -> () with generator_handler[&T]
};
const get = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "list.get"
        | target.name == "javascript" => (list_ref, index) => (
            native "(()=>{let index=$(index);let list=$(list_ref).get();return{get:()=>list[index],set:(value)=>list[index]=value}})()"
        )
    } :: (&list[T], int32) -> &T
};

