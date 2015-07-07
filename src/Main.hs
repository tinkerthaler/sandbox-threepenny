{-----------------------------------------------------------------------------
 Branch of the threepenny-gui CRUD example

 Behaviour a ~= Time -> a
 Event     a ~= [(Time, a)]

 Tidinds consist of facts and rumors
 data Tidings t a = Tidings {
    facts  :: Behaviour t a
    rumors :: Event t a
 }

 [ ] Integrate with qooxdoo.org

 -----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------
    threepenny-gui
    
    Example:
    Small database with CRUD operations and filtering.
    To keep things simple, the list box is rebuild every time
    that the database is updated. This is perfectly fine for rapid prototyping.
    A more sophisticated approach would use incremental updates.
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Prelude hiding (lookup)
import Control.Monad  (void, liftM)
import Data.List      (isPrefixOf)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
-- note: mdo is m-do notation from MonadFix
--       don't yet fully understand it though...
--
-- void: discard the result of the IO action
setup window = void $ mdo
    return window # set title "Lucrum Customer Creation"

    -- GUI elements
    createBtn   <- UI.button #+ [string "Create"]
    saveBtn     <- UI.button #+ [string "Save"]
    cancelBtn   <- UI.button #+ [string "Cancel"]
    deleteBtn   <- UI.button #+ [string "Delete"]
    listBox     <- UI.listBox  bListBoxItems bSelection bDisplayDataItem
    filterEntry <- UI.entry    bFilterString
    ([ customername, firstname, lastname, street, city, zipcode, country ], tDataItem)
                <- dataItem    bSelectionDataItem
    customername2 <- UI.input

    on UI.click saveBtn $ \e -> do 
        customernameValue <- get value customername
        set UI.value customernameValue (element customername2)

    {-
    customername <- UI.input
    firstname    <- UI.input
    lastname     <- UI.input
    street       <- UI.input
    city         <- UI.input
    zipcode      <- UI.input
    country      <- UI.input
    -}

    -- GUI layout
    element listBox # set (attr "size") "10" # set style [("width","200px")]
    
    let uiDataItem = grid [
                           [string "Customer Name:", element customername]
                          ,[string "First Name:" , element firstname]
                          ,[string "Last Name:" , element lastname]
                          ,[string "Street:" , element street]
                          ,[string "City:" , element city]
                          ,[string "Zip Code:" , element zipcode]
                          ,[string "Country:" , element country]
                          ]
    let glue = string " "
    getBody window #+ [grid
                        [
                          [uiDataItem]
                        , [row [ element createBtn, 
                                 element saveBtn, 
                                 element deleteBtn, 
                                 element cancelBtn
                               ], glue]
                        , [row [string "Filter prefix:", element filterEntry], glue]
                        , [element listBox]
                        , [element customername2]
                        --, [uiDataItem]
                        ]
                      ]

    -- events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry
    let tFilter = isPrefixOf <$> UI.userText filterEntry
        bFilter = facts  tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDataItemIn = rumors $ tDataItem
        eCreate     = UI.click createBtn
        eSave       = UI.click saveBtn
        eDelete     = UI.click deleteBtn
        eCancel     = UI.click cancelBtn

    -- database
    let update' mkey x = flip update x <$> mkey
    --bDatabase :: Behavior (Database DataItem)
    bDatabase <- accumB emptydb $ concatenate <$> unions
        [ 
          create emptyDataItem <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        , delete <$> filterJust (bSelection <@ eDelete)
        ]

    -- selection
    --bSelection :: Behavior (Maybe DatabaseKey)
    bSelection <- stepper Nothing $ head <$> unions
        [ eSelection
        , Nothing <$ eDelete
        , Just . nextKey <$> bDatabase <@ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelection <*> bShowDataItem <@> eFilter
        ]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase
                
        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem
        
        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilter <*> bShowDataItem <*> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

    -- automatically enable / disable editing
    let
        bDisplayItem :: Behavior Bool
        bDisplayItem = maybe False (const True) <$> bSelection

    -- sink: the static text value takes a behaviour b that describes the text value
    element deleteBtn    # sink UI.enabled bDisplayItem
    element customername # sink UI.enabled bDisplayItem
    element firstname    # sink UI.enabled bDisplayItem
    element lastname     # sink UI.enabled bDisplayItem
    element street       # sink UI.enabled bDisplayItem
    element city         # sink UI.enabled bDisplayItem
    element zipcode      # sink UI.enabled bDisplayItem
    element country      # sink UI.enabled bDisplayItem

{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptydb = Database 0 Map.empty
keys    = Map.keys . db

create x     (Database newkey db) = Database (newkey+1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey     $ Map.insert key    x db
delete key   (Database newkey db) = Database newkey     $ Map.delete key db
lookup key   (Database _      db) = Map.lookup key db

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}
data DataItem = DataItem {
    customername :: String , 
    firstname    :: String , 
    lastname     :: String , 
    street       :: String , 
    city         :: String , 
    zipcode      :: String , 
    country      :: String
} deriving Show
emptyDataItem = DataItem "" "" "" "" "" "" ""
showDataItem item = (customername item) ++ ", " ++ (lastname item) ++ ", " ++ (firstname item)

-- | Data item widget, consisting of two text entries
dataItem
    :: Behavior (Maybe DataItem)
    -> UI ([Element], Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ customername . maybe emptyDataItem id <$> bItem
    entry2 <- UI.entry $ firstname    . maybe emptyDataItem id <$> bItem
    entry3 <- UI.entry $ lastname     . maybe emptyDataItem id <$> bItem
    entry4 <- UI.entry $ street       . maybe emptyDataItem id <$> bItem
    entry5 <- UI.entry $ city         . maybe emptyDataItem id <$> bItem
    entry6 <- UI.entry $ zipcode      . maybe emptyDataItem id <$> bItem
    entry7 <- UI.entry $ country      . maybe emptyDataItem id <$> bItem
    
    return ( [  getElement entry1, 
                getElement entry2,  
                getElement entry3,  
                getElement entry4, 
                getElement entry5, 
                getElement entry6,  
                getElement entry7 
             ]
           , DataItem       <$> UI.userText entry1  
                            <*> UI.userText entry2 
                            <*> UI.userText entry3 
                            <*> UI.userText entry4 
                            <*> UI.userText entry5 
                            <*> UI.userText entry6 
                            <*> UI.userText entry7
           )
