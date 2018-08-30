# constretto

constretto reads configuration files where each key value is sectioned by tags and key and value separated by `=`

Example, given a file `/home/haskell/project/configuration.ini` :

```
[default]
dbaddress = dev.db.org

[dev]
mykey = devkey

[prod]
dbaddress = prod.db.org 
mykey = prodkey
```

then

    {-# LANGUAGE OverloadedStrings #-} -- to construct a Tag with a String literal
    import Constretto
    import Data.Text

    main = do
        eitherStringConf <- readConfig "/home/testproject/myconfig.ini"
        let res =
                case eitherStringConf of
                Left _ -> "unsuccessful"
                Right conf ->
                    case lookupC ["dev"] "dbaddress" conf of
                    Nothing -> "not found"
                    Just v  -> T.unpack v
        print res

should print out "dev.db.org"

## config file format

## runtime properties

  The runtime property CONSTRETTO_TAGS is supported, where a comma listed set of tags will be searched from right to left in prioritized
  order when looking for keys with 