# Chapter Eight: Configuration and conclusions

[Contents](../README.md)

In this chapter we will tidy things up by adding a clean way of
configuring the server from the previous chapter, and summarise what
was covered in the book. The code for this chapter corresponds to the
`main` branch.

## Configuration

In order to run the game for real we have to host it on a server connected 
to the internet. Such a server will either have a real hostname that can be 
looked up by DNS, or at least an IP address allowing the web client to make
a websocket connection.

The hostname used in development, "127.0.0.1" (the local loopback
address that allows every machine to address itself), is hard-coded
into the server. So is the port and the location of the log file. We'd
like to be able to change these details without needing to recompile
the server. We'll do so in two ways: by reading in a config file and
by accepting options on the command line when the program begins.

## Command-line arguments using `GetOpt`

The main focus for configuration will be via a config file. The only
options we'll allow on the command-line are the location of the config
file and the printing of a brief usage message.

We use the built-in package `System.Console.GetOpt`, which is a wrapper
around the UNIX `getOpt` library. We need to establish a datatype for the
options and a default value for them.

```haskell
-- | Command-line options for the server
newtype Options = Options {
    optConf :: String
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optConf  = "./etc/scrabble.conf"
  }
```

Then we need to define a list of handlers that can consume each
option and set the right value in our `Config` record. As there is only one,
this is easily done.

```haskell
options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "c" ["conf"]
        (ReqArg
            (\arg opt -> return opt { optConf = arg })
            "FILE")
        "Config file"
  , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
  ]
```
Now we can update the `main` function to parse the options.

```haskell
main :: IO ()
main = do
  args <- getArgs
  -- Parse options, getting a list of option actions
  let (actions, _, _) = getOpt RequireOrder options args
  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optConf = path } = opts
  -- rest of main ...
```

Now if we run the server with the `-- help` option we get a usage message.

```
$ cabal run scrabble-server -- --help
Up to date
scrabble-server
  -c FILE  --conf=FILE  Config file
  -h       --help       Show help
```
For a fuller example of using `GetOpt` see 
[this tutorial](https://wiki.haskell.org/High-level_option_handling_with_GetOpt).

## Configuration file parsing using `config-value`

The actual properties we want to configure will be held in a simple
config file of `key:value` pairs with Haskell-style single line
comments. The default for this is `etc/scrabble.conf`, which looks
like this:

```
-- This is the config file for the Scrabble server
-- hostname of the server
hostname: "127.0.0.1"
-- port to run the server on
port:     9001
-- location of the log file
log_file: "log/scrabble.log"
-- log_priority should be one of DEBUG,INFO,NOTICE,WARNING,ERROR,CRITICAL,ALERT,EMERGENCY
log_priority: DEBUG
```
Our config details will be stored in a datatype and we create a record value with the defaults.

```haskell
-- | Container for the config.
data Conf = Conf
  { hostname     :: Text
  , port         :: Int
  , log_file     :: Text
  , log_priority :: Priority
  } deriving Show

-- | The default config for the server. Hostname = "127.0.0.1", port = 9160, log_file
--   = "./log/scrabble.log", log_priority = WARNING.
--   To alter these values edit the config file ./etc/scrabble.conf or write a new config
--   file and supply its location as a command-line option to the server.
defaultConf :: Conf
defaultConf = Conf { hostname = "127.0.0.1"
                   , port = 9160
                   , log_file = "./log/scrabble.log"
                   , log_priority = WARNING }

```

We will use the
[`config-value`](https://hackage.haskell.org/package/config-value)
library and its companion `config-schema` to parse the file.  It has
lots of features but we use it in quite a basic way. We need to parse
two strings (the hostname and the location of the log file), a number
(the port) and the logging priority (a value of `System.Log.Priority`,
which is what we need to supply to `hslogger` when we say what level
of messages we want to record). Each of these should be optional and
we will use the defaults given above if they aren't specified.

The library has parsers for strings and numbers, as well as many other
types, but it doesn't have a parser for the `Priority` type. So we
have to supply a little parser for it and then make `Priority` an
instance of `HasSpec`, the class of types that can be parsed from
config files.

We will require that the priority on the log file is an *atom*, an
identifier with no quotes around it, and use the builtin `atomSpec`
parser.

```haskell
-- | The ValueSpec for Priority specifies how to
--   parse a Priority value.
prioritySpec :: ValueSpec Priority
prioritySpec = DEBUG     <$  atomSpec "DEBUG"
           <!> INFO      <$  atomSpec "INFO"
           <!> NOTICE    <$  atomSpec "NOTICE"
           <!> WARNING   <$  atomSpec "WARNING"
           <!> ERROR     <$  atomSpec "ERROR"
           <!> CRITICAL  <$  atomSpec "CRITICAL"
           <!> ALERT     <$  atomSpec "ALERT"
           <!> EMERGENCY <$  atomSpec "EMERGENCY"

-- | Make Priority an instance of HasSpec.
instance HasSpec Priority where
  anySpec = prioritySpec

```
Now we can specify our config file and use that specification to
parse the file. Since each entry in a config file is optional, we 
use the `optSection` parser, which returns `Maybe a`. We apply `fromMaybe`
to the result along with our default value, so that's what we get if the 
value wasn't defined.

```haskell
-- The spec for the config file.
spec :: Conf -> ValueSpec Conf
spec def = sectionsSpec "scrabble-server conf" $
  do hn <- fromMaybe (hostname def) <$> optSection "hostname"
           "Supply the hostname as a string."
     pt <- fromMaybe (port def) <$> optSection "port" 
           "Supply the port as a number."
     lf <- fromMaybe (log_file def) <$> optSection "log_file"
           "Supply the path to the log file."
     pr <- fromMaybe (log_priority def) <$>  optSection "log_priority"
                 "Supply a value of System.Log.Priority."
     return (Conf hn pt lf pr)

-- Parse the config file with default.
parseConf :: FilePath -> Conf -> IO Conf
parseConf path def = do
  conf <- T.readFile path
  case parse conf of
    Left e  -> usageAndDefault e def
    Right v -> do case loadValue (spec def) v of
                    Left e  -> usageAndDefault e def
                    Right c -> pure c

-- Print the spec for the config file and return the default.
usageAndDefault :: Show a => a -> Conf -> IO Conf
usageAndDefault e def = T.putStrLn (T.pack $ show e)
                        >> printDoc >> T.putStrLn "Using default conf" >> pure def

-- Print the spec for the config file.
printDoc :: IO ()
printDoc = print (generateDocs (spec defaultConf))

```
Now we can finish our changes to the `main` function, parsing the config
file and using its contents to set the logging level and start the server on the
given hostname and port.

```haskell
  conf <- parseConf path defaultConf
  let pr = log_priority conf
  updateGlobalLogger "Scrabble" (setLevel pr)
  h <- fileHandler (T.unpack $ log_file conf) pr >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "Scrabble" (addHandler h)
  infoM "Scrabble" ("Starting server with conf "<>show conf)
  state <- newBoundedChan 2
  _ <- forkIO (gameStarter state)
  WS.runServer (T.unpack $ hostname conf) (port conf) $ enqueue state
```

## Conclusion

From our initial datatypes that modelled the basic idea of letters on
a board, to a webservice that provides a decoupled concurrent API to
our library, we've come a long way. If you've studied the code and
worked on the exercises you have been introduced to a number of
widely-used language extensions and modern idiomatic approaches based
on libraries like `aeson` and `lens-simple`.

The purpose of the book has not really been to explain how to
implement Scrabble in Haskell, but to talk you through the entire
design and implementation of a reasonably-sized project using best
practices. Getting the hang of that involves developing skills at many
different levels: you need an eye for detail, taking advantage of the
strengths of the Haskell language to write code that is both elegant
and correct, and an eye for broader issues of software design,
designing APIs that expose just the right functionality and are nice
to use, protocols that determine robust communication between remote
components of an application, and so on. 

It is sometimes said that the main things a person needs to be a good
programmer are the capacity to jump easily between different levels of
abstraction, and the ability to keep the differing requirements of the
levels in their head simultaneously.

The only way to learn these skills is by practising them, which means
writing a lot of code. It's a continuous process, and one of the great
things about being a programmer is that (like it or not) you never
stop learning. Every now and then you "level up" -- some concept or
technique that you were aware of but never properly understood
suddenly becomes clear, and seems so simple that you wonder how anyone
could fail to see the usefulness of it. This is particularly true if
you're using Haskell! I hope that this book might add to a
levelling-up moment for some reader on the way to understanding
functional problem solving and design.

[Contents](../README.md) 
