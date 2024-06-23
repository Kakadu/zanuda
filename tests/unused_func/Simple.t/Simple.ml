let meow = 1;;

module Meow = struct
  module InnerMeow = struct 
    let inner =  7 
  end
  let woof = 3
  let meow = 11
end

let _false_use = Meow.woof
