@syntax from_scratch;
@syntax comma             1 wrap always = <- _ "," _;
@syntax "trailing comma"  1 wrap always = <- _ ",";
@syntax named             2 wrap always = name "=" value;
@syntax complex          10 wrap always = name "(" children:any ")";