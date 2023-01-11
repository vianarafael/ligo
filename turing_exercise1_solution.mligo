// The smart contract should store a map of usernames (string) and ints associated w/ those usernames 

// 1. The smart contract should be invoked with a string and an int 
// 2. After invokation, the map should be updated as follows:
//     if first invokation -> store the int sent
//     else 
//         if int = 0; int = 1
//         if string exists in map -> update the value of that string 
// int should the int * the int give by the last user

type storage = (string, int) map

type input = {
    key: string;
    value: int
}

type parameter =
    Map_Operation of storage
    // | Send_tez

type return = operation list * storage

// entrypoints

let mapOperation  (store, key, val : storage * input) : storage = 
    let key : string = record_input.key
    let val  = record_intput.value

    // if int = 0 -> int = 1
    let value = if val = 0 then 1
    else record_input.value
    in record_input

    // if map is empty store string and int
    let is_empty : boolean = Map.size(store) < 1n

    if is_empty then Map.add key value store 

    else
    // check if the string already exists in store
    let string_already_in_store =  Map.find_opt key store
    // check for the biggest value in store
    let check_biggest (last_biggest_num, val : int * int) : int =
    if last_biggest_num < val then val
    else last_biggest_num

    let prev_value = Map.fold check_biggest store 0

    if string_already_in_store then Map.update key Some(prev_value * value) store
    else Map.add key (prev_value * value) store

let main (action, store : parameter * storage) =
 ([] : operation list),    // No operations
 (match action with
    Map_Operation (recordInput) -> mapOperation (store, recordInput)
 )

