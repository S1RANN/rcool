run(): String{
    let flag: Bool <- true,
    a: Int <- str2int.a2i(list.get_next().get_item()),
    b: Int <- str2int.a2i(list.get_next().get_next().get_item()) in {
    while flag loop
        {
            io.out_string(">");
            let str: String <- io.in_string() in {
                if str = "d" then
                    list.print(io)
                else if str = "e" then
                    evaluate()
                else if str = "x" then
                    flag <- false
                else
                    list <- list.cons(str)
                fi fi fi;
            };
        }
    pool;
    0;
    }
};