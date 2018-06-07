{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import qualified Data.Aeson                  as A
import qualified Data.Csv                    as Csv
import           Data.Default                (def)
import           Data.Either.Extra           (eitherToMaybe)
import           Data.Hashable               (hash)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           Protolude                   hiding (state)
import           Qi                          (withConfig)
import           Qi.Config.AWS.CW            (CwEventsRuleProfile (ScheduledEventProfile))
import           Qi.Config.AWS.Lambda        (LambdaMemorySize (..),
                                              lpMemorySize, lpTimeoutSeconds)
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (LambdaId, S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, cwEventLambda,
                                              genericLambda, s3Bucket)
import           Qi.Program.Lambda.Interface (CwLambdaProgram,
                                              GenericLambdaProgram,
                                              LambdaProgram, getS3ObjectContent,
                                              invokeLambda, listS3Objects,
                                              putS3ObjectContent, runServant,
                                              say)
import           Qi.Util                     (argumentsError, success)
import qualified Realtor.Api                 as Api
import           Realtor.Item                (Item (Item))
import qualified Realtor.Item                as Item
import           Realtor.Search              (QueryResponse (..), SearchParams,
                                              searchParams)
import qualified Realtor.Search              as Search
import           Realtor.Util


data SearchLocation = SearchLocation {
    city  :: Text
  , state :: Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON SearchLocation
instance A.ToJSON SearchLocation


searchCriteriaLiteral :: SearchLocation -> Text
searchCriteriaLiteral SearchLocation{ city, state } = T.replace " " "-" $ city <> "_" <> state

searchLocations :: [SearchLocation]

searchLocations = [ SearchLocation "Madison" "NJ"
                  , SearchLocation "Chatham" "NJ"
                  , SearchLocation "Summit" "NJ"
                  , SearchLocation "Short Hills" "NJ"
                  , SearchLocation "Millburn" "NJ"
                  , SearchLocation "Maplewood" "NJ"
                  , SearchLocation "South Orange" "NJ"
                  , SearchLocation "Morristown" "NJ"
                  , SearchLocation "ATLANTIC CITY" "NJ"
                  , SearchLocation "ABSECON" "NJ"
                  , SearchLocation "ALLENHURST" "NJ"
                  , SearchLocation "ALLENWOOD" "NJ"
                  , SearchLocation "ALLOWAY" "NJ"
                  , SearchLocation "ANDOVER" "NJ"
                  ]

{-
njCities :: [Text]
njCities = ["ABSECON", "ALLAMUCHY-PANTHER VALLEY", "ALLENDALE", "ALLENHURST", "ALLENTOWN", "ALLENWOOD", "ALLOWAY", "ALPHA", "ALPINE", "ANDOVER", "ANNANDALE", "ASBURY PARK", "ASHLAND", "ATLANTIC CITY", "ATLANTIC HIGHLANDS", "AUDUBON", "AUDUBON PARK", "AVALON", "AVENEL", "AVON-BY-THE-SEA", "BARCLAY-KINGSTON", "BARNEGAT", "BARNEGAT LIGHT", "BARRINGTON", "BAY HEAD", "BAYONNE", "BEACH HAVEN", "BEACH HAVEN WEST", "BEACHWOOD", "BEATYESTOWN", "BECKETT", "BELFORD", "BELLEVILLE", "BELLMAWR", "BELMAR", "BELVIDERE", "BERGENFIELD", "BERKELEY HEIGHTS", "BERLIN", "BERNARDSVILLE", "BEVERLY", "BLACKWOOD", "BLOOMFIELD", "BLOOMINGDALE", "BLOOMSBURY", "BOGOTA", "BOONTON", "BORDENTOWN", "BOUND BROOK", "BRADLEY BEACH", "BRANCHVILLE", "BRASS CASTLE", "BRIDGETON", "BRIELLE", "BRIGANTINE", "BROOKLAWN", "BROWNS MILLS", "BROWNVILLE", "BUDD LAKE", "BUENA", "BURLINGTON", "BUTLER", "CALDWELL", "CALIFON", "CAMDEN", "CAPE MAY", "CAPE MAY COURT HOUSE", "CAPE MAY POINT", "CARLSTADT", "CARNEYS POINT", "CARTERET", "CEDAR GLEN LAKES", "CEDAR GLEN WEST", "CEDAR GROVE", "CEDARVILLE", "CHATHAM", "CHERRY HILL MALL", "CHESILHURST", "CHESTER", "CLARK", "CLAYTON", "CLEARBROOK PARK", "CLEMENTON", "CLIFFSIDE PARK", "CLIFFWOOD BEACH", "CLIFTON", "CLINTON", "CLOSTER", "COLLINGS LAKES", "COLLINGSWOOD", "COLONIA", "CONCORDIA", "CORBIN CITY", "COUNTRY LAKE ESTATES", "CRANBURY", "CRANDON LAKES", "CRANFORD", "CRESSKILL", "CRESTWOOD VILLAGE", "DAYTON", "DEAL", "DEMAREST", "DIAMOND BEACH", "DOVER", "DOVER BEACHES NORTH", "DOVER BEACHES SOUTH", "DUMONT", "DUNELLEN", "EAST BRUNSWICK", "EAST FREEHOLD", "EAST NEWARK", "EAST ORANGE", "EAST RUTHERFORD", "EATONTOWN", "ECHELON", "EDGEWATER", "EDISON", "EGG HARBOR CITY", "ELIZABETH", "ELMER", "ELMWOOD PARK", "ELWOOD-MAGNOLIA", "EMERSON", "ENGLEWOOD", "ENGLEWOOD CLIFFS", "ENGLISHTOWN", "ERLTON-ELLISBURG", "ERMA", "ESSEX FELLS", "ESTELL MANOR", "EWING", "FAIRFIELD", "FAIR HAVEN", "FAIR LAWN", "FAIRTON", "FAIRVIEW", "FAIRVIEW", "FANWOOD", "FAR HILLS", "FARMINGDALE", "FIELDSBORO", "FLEMINGTON", "FLORENCE-ROEBLING", "FLORHAM PARK", "FOLSOM", "FORDS", "FORKED RIVER", "FORT DIX", "FORT LEE", "FRANKLIN", "FRANKLIN LAKES", "FREEHOLD", "FRENCHTOWN", "GARFIELD", "GARWOOD", "GIBBSBORO", "GIBBSTOWN", "GLASSBORO", "GLENDORA", "GLEN GARDNER", "GLEN RIDGE", "GLEN ROCK", "GLOUCESTER CITY", "GOLDEN TRIANGLE", "GREAT MEADOWS-VIENNA", "GREENTREE", "GUTTENBERG", "HACKENSACK", "HACKETTSTOWN", "HADDONFIELD", "HADDON HEIGHTS", "HALEDON", "HAMBURG", "HAMMONTON", "HAMPTON", "HARRINGTON PARK", "HARRISON", "HARVEY CEDARS", "HASBROUCK HEIGHTS", "HAWORTH", "HAWTHORNE", "HEATHCOTE", "HELMETTA", "HIGH BRIDGE", "HIGHLAND LAKE", "HIGHLAND PARK", "HIGHLANDS", "HIGHTSTOWN", "HILLSDALE", "HILLSIDE", "HI-NELLA", "HOBOKEN", "HO-HO-KUS", "HOLIDAY CITY-BERKELEY", "HOLIDAY CITY SOUTH", "HOLIDAY HEIGHTS", "HOPATCONG", "HOPEWELL", "INTERLAKEN", "IRVINGTON", "ISELIN", "ISLAND HEIGHTS", "JAMESBURG", "JERSEY CITY", "KEANSBURG", "KEARNY", "KENDALL PARK", "KENILWORTH", "KEYPORT", "KINGSTON", "KINNELON", "LAKEHURST", "LAKE MOHAWK", "LAKE TELEMARK", "LAKEWOOD", "LAMBERTVILLE", "LAUREL LAKE", "LAUREL SPRINGS", "LAURENCE HARBOR", "LAVALLETTE", "LAWNSIDE", "LAWRENCEVILLE", "LEBANON", "LEISURE KNOLL", "LEISURETOWNE", "LEISURE VILLAGE", "LEISURE VILLAGE EAST", "LEISURE VILLAGE WEST-PINE LAKE PARK", "LEONARDO", "LEONIA", "LINCOLN PARK", "LINCROFT", "LINDEN", "LINDENWOLD", "LINWOOD", "LITTLE FALLS", "LITTLE FERRY", "LITTLE SILVER", "LIVINGSTON", "LOCH ARBOUR", "LODI", "LONG BRANCH", "LONGPORT", "LONG VALLEY", "LYNDHURST", "MCGUIRE AFB", "MADISON", "MADISON PARK", "MAGNOLIA", "MANAHAWKIN", "MANASQUAN", "MANTOLOKING", "MANVILLE", "MAPLEWOOD", "MARGATE CITY", "MARLTON", "MATAWAN", "MAYS LANDING", "MAYWOOD", "MEDFORD LAKES", "MENDHAM", "MERCERVILLE-HAMILTON SQUARE", "MERCHANTVILLE", "METUCHEN", "MIDDLESEX", "MIDLAND PARK", "MILFORD", "MILLBURN", "MILLSTONE", "MILLTOWN", "MILLVILLE", "MONMOUTH BEACH", "MONMOUTH JUNCTION", "MONTCLAIR", "MONTVALE", "MOONACHIE", "MOORESTOWN-LENOLA", "MORGANVILLE", "MORRIS PLAINS", "MORRISTOWN", "MOUNTAIN LAKES", "MOUNTAINSIDE", "MOUNT ARLINGTON", "MOUNT EPHRAIM", "MULLICA HILL", "MYSTIC ISLAND", "NATIONAL PARK", "NAVESINK", "NEPTUNE CITY", "NETCONG", "NEWARK", "NEW BRUNSWICK", "NEW EGYPT", "NEWFIELD", "NEW MILFORD", "NEW PROVIDENCE", "NEWTON", "NORTH ARLINGTON", "NORTH BEACH HAVEN", "NORTH BRUNSWICK TOWNSHIP", "NORTH CALDWELL", "NORTH CAPE MAY", "NORTHFIELD", "NORTH HALEDON", "NORTH MIDDLETOWN", "NORTH PLAINFIELD", "NORTHVALE", "NORTH WILDWOOD", "NORWOOD", "NUTLEY", "OAKHURST", "OAKLAND", "OAKLYN", "OAK VALLEY", "OCEAN ACRES", "OCEAN CITY", "OCEAN GATE", "OCEAN GROVE", "OCEANPORT", "OGDENSBURG", "OLD BRIDGE", "OLD TAPPAN", "OLIVET", "ORADELL", "ORANGE", "OXFORD", "PALISADES PARK", "PALMYRA", "PARAMUS", "PARK RIDGE", "PASSAIC", "PATERSON", "PAULSBORO", "PEAPACK AND GLADSTONE", "PEMBERTON", "PEMBERTON HEIGHTS", "PENNINGTON", "PENNSAUKEN", "PENNS GROVE", "PENNSVILLE", "PERTH AMBOY", "PHILLIPSBURG", "PINE BEACH", "PINE HILL", "PINE RIDGE AT CRESTWOOD", "PINE VALLEY", "PITMAN", "PLAINFIELD", "PLAINSBORO CENTER", "PLEASANTVILLE", "POINT PLEASANT", "POINT PLEASANT BEACH", "POMONA", "POMPTON LAKES", "PORT MONMOUTH", "PORT NORRIS", "PORT READING", "PORT REPUBLIC", "PRESIDENTIAL LAKES ESTATES", "PRINCETON", "PRINCETON JUNCTION", "PRINCETON MEADOWS", "PRINCETON NORTH", "PROSPECT PARK", "RAHWAY", "RAMBLEWOOD", "RAMSEY", "RAMTOWN", "RARITAN", "RED BANK", "RIDGEFIELD", "RIDGEFIELD PARK", "RIDGEWOOD", "RINGWOOD", "RIO GRANDE", "RIVERDALE", "RIVER EDGE", "RIVERTON", "RIVER VALE", "ROCHELLE PARK", "ROCKAWAY", "ROCKLEIGH", "ROCKY HILL", "ROOSEVELT", "ROSELAND", "ROSELLE", "ROSELLE PARK", "ROSENHAYN", "ROSSMOOR", "RUMSON", "RUNNEMEDE", "RUTHERFORD", "SADDLE BROOK", "SADDLE RIVER", "SALEM", "SAYREVILLE", "SCOTCH PLAINS", "SEA BRIGHT", "SEABROOK FARMS", "SEA GIRT", "SEA ISLE CITY", "SEASIDE HEIGHTS", "SEASIDE PARK", "SECAUCUS", "SEWAREN", "SHARK RIVER HILLS", "SHILOH", "SHIP BOTTOM", "SHREWSBURY", "SILVER RIDGE", "SOCIETY HILL", "SOMERDALE", "SOMERSET", "SOMERS POINT", "SOMERVILLE", "SOUTH AMBOY", "SOUTH BELMAR", "SOUTH BOUND BROOK", "SOUTH ORANGE", "SOUTH PLAINFIELD", "SOUTH RIVER", "SOUTH TOMS RIVER", "SPOTSWOOD", "SPRINGDALE", "SPRINGFIELD", "SPRING LAKE", "SPRING LAKE HEIGHTS", "STANHOPE", "STOCKTON", "STONE HARBOR", "STRATFORD", "STRATHMERE", "STRATHMORE", "SUCCASUNNA-KENVIL", "SUMMIT", "SURF CITY", "SUSSEX", "SWEDESBORO", "TAVISTOCK", "TEANECK", "TENAFLY", "TETERBORO", "TINTON FALLS", "TOMS RIVER", "TOTOWA", "TRENTON", "TUCKERTON", "TURNERSVILLE", "TWIN RIVERS", "UNION", "UNION BEACH", "UNION CITY", "UPPER SADDLE RIVER", "VENTNOR CITY", "VERNON VALLEY", "VERONA", "VICTORY GARDENS", "VICTORY LAKES", "VILLAS", "VINELAND", "VISTA CENTER", "WALDWICK", "WALLINGTON", "WANAMASSA", "WANAQUE", "WARETOWN", "WASHINGTON", "WASHINGTON TOWNSHIP", "WATCHUNG", "WAYNE", "WENONAH", "WEST BELMAR", "WEST CALDWELL", "WEST CAPE MAY", "WESTFIELD", "WEST FREEHOLD", "WEST LONG BRANCH", "WEST MILFORD", "WEST NEW YORK", "WEST ORANGE", "WEST PATERSON", "WESTVILLE", "WEST WILDWOOD", "WESTWOOD", "WHARTON", "WHITE HORSE", "WHITE HOUSE STATION", "WHITE MEADOW LAKE", "WHITESBORO-BURLEIGH", "WHITTINGHAM", "WILDWOOD", "WILDWOOD CREST", "WILLIAMSTOWN", "WOODBINE", "WOODBRIDGE", "WOODBURY", "WOODBURY HEIGHTS", "WOODCLIFF LAKE", "WOODLYNNE", "WOOD-RIDGE", "WOODSTOWN", "WRIGHTSTOWN", "WYCKOFF", "YARDVILLE-GROVEVILLE", "YORKETOWN"]

searchLocations = (`SearchLocation` "NJ") <$> njCities
-}


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      let fetchRecentlySoldCron = ScheduledEventProfile "cron(0 1 * * ? *)"
          fetchForSaleCron      = ScheduledEventProfile "cron(0 2 * * ? *)"
          extractCron           = ScheduledEventProfile "cron(0 0 * * ? *)"
          defaultLambdaProfile = def & lpMemorySize     .~ M512
                                     & lpTimeoutSeconds .~ 300

      recentlySoldBucketId  <- s3Bucket "recently-sold"
      forSaleBucketId       <- s3Bucket "for-sale"
      outputBucketId        <- s3Bucket "output"

      fetchRecentlySoldLocationLambdaId <- genericLambda
                                        "fetchRecentlySoldLocation"
                                        (fetchLocationLambda recentlySoldBucketId "Search::RecentlySoldController")
                                        defaultLambdaProfile
      cwEventLambda
        "fetchRecentlySold"
        fetchRecentlySoldCron
        (fetchLambda fetchRecentlySoldLocationLambdaId)
        defaultLambdaProfile

      fetchForSaleLocationLambdaId <- genericLambda
                                "fetchForSaleLocation"
                                (fetchLocationLambda forSaleBucketId "Search::PropertiesController")
                                defaultLambdaProfile
      cwEventLambda
        "fetchForSale"
        fetchForSaleCron
        (fetchLambda fetchForSaleLocationLambdaId)
        defaultLambdaProfile


      cwEventLambda
        "extractRecentlySold"
        extractCron
        (extractItemsLambda recentlySoldBucketId outputBucketId "recently-sold.csv")
        defaultLambdaProfile

      cwEventLambda
        "extractForSale"
        extractCron
        (extractItemsLambda forSaleBucketId outputBucketId "for-sale.csv")
        defaultLambdaProfile

      pass


    extractItemsLambda
      :: S3BucketId
      -> S3BucketId
      -> Text
      -> CwLambdaProgram
    extractItemsLambda fromBuckedId toBucketId filename = \_ -> do
      objs <- listS3Objects fromBuckedId $ \acc objs -> pure $ acc V.++ objs
      (items :: Vector Item) <- V.mapMaybe eitherToMaybe <$> traverse (map (\ebs -> (first toS ebs) >>= A.eitherDecode) . getS3ObjectContent) objs
      putS3ObjectContent (S3Object toBucketId $ S3Key filename) . Csv.encodeByName Item.headers $ V.toList items
      success "success!"


    fetchLambda
      :: LambdaId
      -> CwLambdaProgram
    fetchLambda fetchLocationLambdaId = \_ -> do
      traverse_ (invokeLambda fetchLocationLambdaId) searchLocations
      success "success!"


    fetchLocationLambda
      :: S3BucketId
      -> Text
      -> GenericLambdaProgram
    fetchLocationLambda bucketId controllerName = \json -> do
      case A.eitherDecode $ A.encode json of
        Left err -> argumentsError $ toS err
        Right sl@SearchLocation{ city, state } -> do
          let criteria = searchCriteriaLiteral sl
              params = searchParams criteria controllerName
          persistedPages <- persistPages Api.fetch params (persistItem bucketId) 1
          say $ "persisted " <> show persistedPages <> " pages for: '" <> criteria <> "'"

          success "success!"
