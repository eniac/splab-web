{-# LANGUAGE OverloadedStrings #-}

{-|
 - Module: PLClub
 - Description: Top-level module for PLClub website
 -}

module PLClub where

--------------------------------------------------------------------------------
import           Data.Monoid (mappend)
import           Hakyll
import qualified Hakyll.Alectryon as Alectryon (hakyllWith)
import           PLClub.Publications
import           PLClub.HakyllExtra
import           PLClub.PandocExtra

-- | Compose routes, renamed to emphasize
-- that LHS is applied before RHS
(<!>), thenRoute :: Routes -> Routes -> Routes
(<!>) = composeRoutes
thenRoute = composeRoutes

config :: Configuration
config = defaultConfiguration
  { deployCommand = "./extra/deploy.sh"
  }

--------------------------------------------------------------------------------
application :: IO ()
application = Alectryon.hakyllWith config $ \opts -> do
    match "img/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "vendor/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match "people/*" $ do
        compile getResourceBody

    match "meetings/*" $ do
        route   $ setExtension "html" <!> makeIntoFolder
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/meeting.html" siteContext
                >>= loadAndApplyTemplate "templates/default.html"  siteContext
                >>= relativizeUrls

    match "extra/syntax/*.theme" $ do
      route   $ flattenIntoFolder "css" <!> setExtension "css"
      compile $ kateThemeToCSSCompiler

    --people tags
    ptags <- buildTags "people/*" (fromCapture "ptags/*.html")

    create ["papers.html"] $ do
        route   $ makeIntoFolder
        compile $ do
            let ctx =
                    papersContext `mappend`
                    constField "title" "PLClub Publications" `mappend`
                    siteContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" siteContext
                >>= relativizeUrls

    create ["papers/plclub_bib.html"] $ do
        route   $ idRoute
        compile $ do
            makeItem =<< unsafeCompiler makeBibHtml

    match "club.html" $ do
        route   $  makeIntoFolder
        compile $ do
            meetings <- recentFirst =<< loadAll "meetings/*"
            let meetingsCtx =
                    listField "meetings" siteContext (return meetings) `mappend`
                    constField "title" "PLClub Discussion Group" `mappend`
                    siteContext
            getResourceBody
                >>= applyAsTemplate meetingsCtx
                >>= loadAndApplyTemplate "templates/default.html" meetingsCtx
                >>= relativizeUrls

    match "old_site/**" $ do
      route   $ routeTail <!> htaccessHackRoute
      compile $ copyFileCompiler

    match "index.html" $ do
        rulesExtraDependencies [tagsDependency ptags] $ do
            route idRoute
            compile $ do
                meetings <- recentFirst =<< loadAll "meetings/*"
                let indexCtx =
                        peopleContext ptags `mappend`
                        listField "meetings" siteContext (return meetings) `mappend`
                        constField "title" "Security and Privacy Lab" `mappend`
                        recentPapersContext `mappend`
                        siteContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
unbindList :: Int -> [a] -> [[a]]
unbindList _ [] = []
unbindList n as =
    (take n as):(unbindList n $ drop n as)

peopleContext :: Tags -> Context String
peopleContext ptags =
  let faculty  = (unbindList 3) <$> loadTag ptags "faculty" :: Compiler [[Item String]]
      affiliated = (unbindList 3) <$> loadTag ptags "affiliated" :: Compiler [[Item String]]
      students = (unbindList 3) <$> loadTag ptags "student" :: Compiler [[Item String]]
      postdocs = (unbindList 3) <$> loadTag ptags "postdoc" :: Compiler [[Item String]]
      alum'    = loadTag ptags "alum" :: Compiler [Item String]
      alum     = reverse <$> (sortOnM getYear =<< alum')
  in
    nestedListField "facultyGroup" "faculty" siteContext faculty `mappend`
    nestedListField "affiliatedGroup" "affiliated" siteContext affiliated `mappend`
    nestedListField "studentGroup" "student" siteContext students`mappend`
    nestedListField "postdocGroup" "postdoc" siteContext postdocs`mappend`
    listField "alum" siteContext alum
