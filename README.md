This is a small and simple template that shows how to use Hakyll with SASS and Foundation.

After changing the Haskell code:

    stack build

After changing the content of the site:

    stack exec site build

Start wach mode:

    stack exec site watch

(The site is served at http://127.0.0.1:8000 )

When you encounter problems:

    stack exec site rebuild

The generated site is found in the _site directory. You can transfer its content to a HTML-server.

