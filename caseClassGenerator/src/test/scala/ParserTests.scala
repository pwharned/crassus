import org.pwharned.{SQLParser, SqlDataType, SqlString}
object ParserTests extends  App {




  val actions  = """
                   |CREATE TABLE public.actions (
                   |    action_id uuid DEFAULT gen_random_uuid() NOT NULL,
                   |    action_user text NOT NULL,
                   |    created_at timestamp with time zone DEFAULT now() NOT NULL,
                   |    related_asset uuid NOT NULL,
                   |    action_type text NOT NULL
                   |)""".stripMargin

  SQLParser.createTableParser(actions) match {
    case Left(value) => println(value)
    case Right(value) => println(value)
  }

  val alterTable =
    """
ALTER TABLE ONLY public.products ADD CONSTRAINT products_pkey PRIMARY KEY (product_id);
      |
      |""".stripMargin

  SQLParser.alterTablePrimaryKeyParser(alterTable) match {
    case Left(value) => println(value)
    case Right(value) => println(value)
  }

  SQLParser.defaultParser("DEFAULT 0.0") match {
    case Left(value) => println(value)
    case Right(value) => println(value)
  }
  SQLParser.columnparser(" asset_market text") match {
    case Left(value) => print(value)
    case Right(value) => println(value)
  }

  val assets = """CREATE TABLE public.assets (
                 |    asset_id uuid DEFAULT gen_random_uuid() NOT NULL,
                 |    asset_name text NOT NULL,
                 |    asset_owner text NOT NULL,
                 |    asset_description text NOT NULL,
                 |    asset_type text NOT NULL,
                 |    asset_link text NOT NULL,
                 |    created_at timestamp with time zone DEFAULT now() NOT NULL,
                 |    updated_at timestamp with time zone DEFAULT now(),
                 |    asset_offering_type text,
                 |    asset_brand text,
                 |    asset_practice text,
                 |    is_ip_cleared boolean DEFAULT false,
                 |    is_sellable boolean DEFAULT false,
                 |    asset_rating_avg double precision DEFAULT 0.0,
                 |    asset_collaborators text[],
                 |    asset_owner_name text NOT NULL,
                 |    asset_geo text,
                 |    asset_market text
                 |)""".stripMargin
  SQLParser.createTableParser(assets) match {
    case Left(value) => println(value)
    case Right(value) => println(value)
  }

  SqlString.parse("character varying(255)") match {
    case Left(value) => println(value)
    case Right(value) => println(value)
  }

}
