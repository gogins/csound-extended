Module['ENIVIRONMENT']='WEB';
/**
 * So that print output works in all JavaScript contexts, not only 
 * browsers, we set up our own print function to handle all cases.
 */
var print = null;
if (typeof console === 'undefined') {
    print = Module.print;
} else print = function(message) {
    console.log(message);
    //Module.print(message);
}
