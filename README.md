# Konstretto

Constretto reads configuration files where each key value is sectioned by tags and key and value separated by `=`
and is highly inspired by the config file format of the apache constretto library

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
    import Konstretto
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

or running with TAGS=dev

    main :: IO ()
    main = do
    adrE <- lookupFromEnvAndFile "TAGS" "/home/testproject/myconfig.ini" "dbaddress"
    case adrE of
        Right adr -> print adr
        _ -> error "need db address to start app"


should print out "dev.db.org"

## Config file format

* at least one Tag in the form of `[exampletag]`
* any number of `key=value` under each tag
* any number of sections consisting of a tag + key=value
* no comments support at the moment
* a [default] tag is special - it is a default fallback for key/vals. It has the lowest priority when searching using multiple tags.

## Searching for keys/vals

* search using a comma separated list narrowing down the tags in descending order of priority and falling back to any [default]-tag

## Runtime properties

A searchable runtime property is supported, where a comma listed set of tags will be searched from right to left in prioritized
order when looking for keys.

Example, if your application is started with the property TAGS like this

    stack exec --docker-run-args='-e TAGS=prod' -- myapp

The configuration file is searched for the tag [prod]. The property can be comma-separated, and is searched from right to left in descending priority when selecting a key/value.