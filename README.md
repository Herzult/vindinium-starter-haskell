Vindinium Starter Haskell
=========================

Haskell starter for [Vindinium](http://vindinium.org).

Creating Your Bot
-----------------

You can create your bot by editing the `Bot.bot` function.

Executing Your Bot
------------------

First install [haskell stack](https://haskell-lang.org/get-started). Next you should just need to run `stack build` to download dependencies and build the project.

Once that is done, you can run your bot as follows:

```
stack exec -- vindinium training <API key> [--turns 10] [--url http://custom.vindinium.url]
stack exec -- vindinium arena <API key> [--url http://custom.vindinium.url]
```
