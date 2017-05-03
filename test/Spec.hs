import Data.Aeson (Value)
import qualified Data.Aeson as Json
import JsonDiff
import Protolude

main :: IO ()
main = do
  putText $ prettyDiff $ diffStructures obj1 obj2

obj1 :: Value
obj1 =
  (\(Just x) -> x) $
  Json.decode
    "[{\"serviceId\": 0, \"serviceName\": \"string\", \"requiredServiceDataItems\": [{\"profileDataFieldItem\": {\"fieldOwnerType\": \"ACCOUNT\", \"itemId\": \"USER_ID\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\"]}, \"profileDataFieldGroup\": {\"itemId\": \"string\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\"]}}], \"requiredCustomProperties\": [\"string\"]}]"

obj2 :: Value
obj2 =
  (\(Just x) -> x) $
  Json.decode
    "[{\"serviceId\": 0, \"serviceName\": \"string\", \"requiredServiceDataItems\": [{\"profileDataFieldItem\": {\"fieldOwnerType\": \"ACCOUNT\", \"itemId\": \"USER_ID\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\", \"shouldNotBeFound\"]}, \"profileDataFieldGroup\": {\"itemId\": \"string\", \"profileDataFieldRelationshipType\": {\"test\": 15}, \"childrenIds\": [\"USER_ID\"]}}], \"requiredCustomProperties\": [15]}]"

test1 :: Value
test1 =
  (\(Just x) -> x) $
  Json.decode
    "[{\"serviceId\": 0, \"serviceName\": \"string\", \"requiredServiceDataItems\": [{\"profileDataFieldItem\": {\"fieldOwnerType\": \"ACCOUNT\", \"itemId\": \"USER_ID\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\"]}, \"profileDataFieldGroup\": {\"itemId\": \"string\", \"profileDataFieldRelationshipType\": \"GROUP\", \"childrenIds\": [\"USER_ID\"]}}], \"requiredCustomProperties\": [\"string\"]}]"

test2 :: Value
test2 =
  (\(Just x) -> x) $
  Json.decode
    " [{\"serviceId\":1,\"requiredServiceDataItems\":[{\"profileDataFieldItem\":null,\"profileDataFieldGroup\":{\"profileDataFieldRelationshipType\":\"GROUP\",\"childrenIds\":[\"IS_CONTACTED_BY_EMAIL\",\"IS_CONTACTED_BY_PHONE\",\"IS_CONTACTED_BY_LETTER\"],\"itemId\":\"COMMUNICATION_CHANNEL\"}},{\"profileDataFieldItem\":{\"profileDataFieldRelationshipType\":\"DATAFIELD\",\"childrenIds\":[\"LANDLINE_PHONE\",\"MOBILE_PHONE\"],\"fieldOwnerType\":\"ACCOUNT\",\"itemId\":\"IS_CONTACTED_BY_PHONE\"},\"profileDataFieldGroup\":null},{\"profileDataFieldItem\":{\"profileDataFieldRelationshipType\":\"DATAFIELD\",\"childrenIds\":null,\"fieldOwnerType\":\"ACCOUNT\",\"itemId\":\"USER_ID\"},\"profileDataFieldGroup\":null}],\"requiredCustomProperties\":[]}] "
