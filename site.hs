--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompilerWith)
import Text.Sass.Options
import qualified Bindings.Libsass    as Lib

saasOptions = SassOptions
      { sassPrecision         = 5
      , sassOutputStyle       = Lib.SassStyleNested
      , sassSourceComments    = False
      , sassSourceMapEmbed    = False
      , sassSourceMapContents = False
      , sassOmitSourceMapUrl  = False
      , sassIsIndentedSyntax  = False
      , sassIndent            = "  "
      , sassLinefeed          = "\n"
      , sassInputPath         = Nothing
      , sassOutputPath        = Nothing
      , sassPluginPaths       = Nothing
      , sassIncludePaths      = Just ["./scss/3rdParty/foundation/", "./scss/project/"]
      , sassSourceMapFile     = Nothing
      , sassSourceMapRoot     = Nothing
      , sassFunctions         = Nothing
      , sassHeaders           = Nothing
      , sassImporters         = Nothing
      }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "htmls/*.html" $ do
            route $ gsubRoute "htmls/" $ const ""
            compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/*.html" $ do
        route $ gsubRoute "pages/" $ const ""
        compile $ do
            getResourceBody
                >>= applyAsTemplate standardCtx
                >>= loadAndApplyTemplate "templates/default.html" standardCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    match "scss/project/styling.scss" $ do
        route $ constRoute "css/styling.css"
        compile $ sassCompilerWith saasOptions


--------------------------------------------------------------------------------
standardCtx :: Context String
standardCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
