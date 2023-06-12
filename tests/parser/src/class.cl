class List{
    item: String;
    next: List;

    is_nil(): Bool { true };

    get_item(): String { item };

    get_next(): List { next };

    set_item(in_item: String): String { item <- in_item };
    
    init(str: String, rest: List): List{
        {
            item <- str;
            next <- rest;
            self;
        }
    };

    cons(str: String): List{
        (new Cons).init(str, self)
    };

    print(io: IO): Int{
        if not is_nil() then
            {
                io.out_string(item);
                io.out_string("\n");
                next.print(io);
            }
        else 
            0
        fi
    };
};