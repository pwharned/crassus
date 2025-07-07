
package generated
import org.pwharned.database.HKD._
import java.sql.Timestamp


case class actions[F[_]] ( action_id: F[PrimaryKey[java.util.UUID]],action_user: F[String],created_at: F[String],related_asset: F[java.util.UUID],action_type: F[String])

case class asset_bookmarks[F[_]] ( asset_id: F[PrimaryKey[java.util.UUID]], email: F[PrimaryKey[String]])

case class asset_collection[F[_]] ( asset_id: F[PrimaryKey[java.util.UUID]], collection_id: F[PrimaryKey[java.util.UUID]])

case class asset_product[F[_]] ( asset_id: F[PrimaryKey[java.util.UUID]], product_id: F[PrimaryKey[String]])

case class asset_ratings[F[_]] ( rating_id: F[PrimaryKey[java.util.UUID]],rating_value: F[Float],createdby: F[String],related_asset: F[java.util.UUID])

case class asset_types[F[_]] ( type_id: F[PrimaryKey[String]],type_name: F[String])

case class assets[F[_]] ( asset_id: F[PrimaryKey[java.util.UUID]],asset_name: F[String],asset_owner: F[String],asset_description: F[String],asset_type: F[String],asset_link: F[String],created_at: F[String],updated_at: F[String],asset_offering_type: F[String],asset_brand: F[String],asset_practice: F[String],is_ip_cleared: F[Boolean],is_sellable: F[Boolean],asset_rating_avg: F[Float],asset_collaborators: F[List[String]],asset_owner_name: F[String],asset_geo: F[String],asset_market: F[String])

case class attributes[F[_]] ( id: F[PrimaryKey[Int]],name: F[String])

case class attributevalues[F[_]] ( id: F[PrimaryKey[Int]],aid: F[Int],value: F[String])

case class brands[F[_]] ( brand_id: F[PrimaryKey[String]],brand_name: F[String])

case class collections[F[_]] ( collection_id: F[PrimaryKey[java.util.UUID]],collection_name: F[String],created_at: F[String],updated_at: F[String],collection_description: F[String],collection_owner: F[String],collection_collaborators: F[List[String]],collection_owner_name: F[String])

case class comments[F[_]] ( comment_id: F[PrimaryKey[java.util.UUID]],comment_value: F[String],item_id: F[java.util.UUID],created_by: F[String],created_at: F[String],creator_name: F[String],updated_at: F[String])

case class entities[F[_]] ( id: F[PrimaryKey[Int]],name: F[String])

case class entityattributes[F[_]] ( eid: F[PrimaryKey[java.util.UUID]], aid: F[PrimaryKey[Int]], vid: F[PrimaryKey[Int]])

case class nominations[F[_]] ( nomination_id: F[PrimaryKey[java.util.UUID]],asset_id: F[java.util.UUID],nominator: F[String],features: F[String],impact: F[String],evidence: F[String],conclusion: F[String],created_at: F[String],status: F[String],nominator_name: F[String])

case class offering_types[F[_]] ( offering_type_id: F[PrimaryKey[String]],offering_type_name: F[String])

case class parent[F[_]] ( id: F[PrimaryKey[Int]],paid: F[Int],caid: F[Int])

case class practices[F[_]] ( practice_id: F[PrimaryKey[String]],practice_name: F[String],owning_brand: F[String])

case class products[F[_]] ( product_id: F[PrimaryKey[String]],product_name: F[String])

case class relationship[F[_]] ( id: F[PrimaryKey[Int]],paid: F[Int],caid: F[Int],pavid: F[Int],cavid: F[Int])
