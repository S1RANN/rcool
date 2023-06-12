case condition of
    x : Int => io.print("x is Int");
    y : String => {
        let x: String <- y.concat("\n") in io.print(x);
        y;
    };
    z : Bool => if z then
                    1
                else
                    2
                fi;
esac