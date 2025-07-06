


CREATE TABLE public.actions (
    action_id uuid DEFAULT gen_random_uuid() NOT NULL,
    action_user text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    related_asset uuid NOT NULL,
    action_type text NOT NULL
);




CREATE TABLE public.asset_bookmarks (
    asset_id uuid NOT NULL,
    email text NOT NULL
);




CREATE TABLE public.asset_collection (
    asset_id uuid NOT NULL,
    collection_id uuid NOT NULL
);




CREATE TABLE public.asset_product (
    asset_id uuid NOT NULL,
    product_id text NOT NULL
);


ALTER TABLE public.asset_product OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.asset_ratings (
    rating_id uuid DEFAULT gen_random_uuid() NOT NULL,
    rating_value double precision NOT NULL,
    createdby text NOT NULL,
    related_asset uuid NOT NULL
);


ALTER TABLE public.asset_ratings OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.asset_types (
    type_id text NOT NULL,
    type_name text NOT NULL
);


ALTER TABLE public.asset_types OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.assets (
    asset_id uuid DEFAULT gen_random_uuid() NOT NULL,
    asset_name text NOT NULL,
    asset_owner text NOT NULL,
    asset_description text NOT NULL,
    asset_type text NOT NULL,
    asset_link text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now(),
    asset_offering_type text,
    asset_brand text,
    asset_practice text,
    is_ip_cleared boolean DEFAULT false,
    is_sellable boolean DEFAULT false,
    asset_rating_avg double precision DEFAULT 0.0,
    asset_collaborators text[],
    asset_owner_name text NOT NULL,
    asset_geo text,
    asset_market text
);


ALTER TABLE public.assets OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.attributes (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE public.attributes OWNER TO "ibm-cloud-base-user";


ALTER TABLE public.attributes ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.attributes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE public.attributevalues (
    id integer NOT NULL,
    aid integer NOT NULL,
    value character varying(255) NOT NULL
);


ALTER TABLE public.attributevalues OWNER TO "ibm-cloud-base-user";


ALTER TABLE public.attributevalues ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.attributevalues_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE public.brands (
    brand_id text NOT NULL,
    brand_name text NOT NULL
);


ALTER TABLE public.brands OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.collections (
    collection_id uuid DEFAULT gen_random_uuid() NOT NULL,
    collection_name text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    collection_description text NOT NULL,
    collection_owner text NOT NULL,
    collection_collaborators text[],
    collection_owner_name text NOT NULL
);


ALTER TABLE public.collections OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.comments (
    comment_id uuid DEFAULT gen_random_uuid() NOT NULL,
    comment_value text NOT NULL,
    item_id uuid NOT NULL,
    created_by text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    creator_name text NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.comments OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.entities (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE public.entities OWNER TO "ibm-cloud-base-user";


ALTER TABLE public.entities ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.entities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE public.entityattributes (
    eid uuid NOT NULL,
    aid integer NOT NULL,
    vid integer NOT NULL
);


ALTER TABLE public.entityattributes OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.nominations (
    nomination_id uuid DEFAULT gen_random_uuid() NOT NULL,
    asset_id uuid NOT NULL,
    nominator text NOT NULL,
    features text NOT NULL,
    impact text NOT NULL,
    evidence text NOT NULL,
    conclusion text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    status text NOT NULL,
    nominator_name text NOT NULL
);


ALTER TABLE public.nominations OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.offering_types (
    offering_type_id text NOT NULL,
    offering_type_name text NOT NULL
);


ALTER TABLE public.offering_types OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.parent (
    id integer NOT NULL,
    paid integer NOT NULL,
    caid integer NOT NULL
);


ALTER TABLE public.parent OWNER TO "ibm-cloud-base-user";


ALTER TABLE public.parent ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.parent_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



CREATE TABLE public.practices (
    practice_id text NOT NULL,
    practice_name text NOT NULL,
    owning_brand text NOT NULL
);


ALTER TABLE public.practices OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.products (
    product_id text NOT NULL,
    product_name text NOT NULL
);


ALTER TABLE public.products OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.relationship (
    id integer NOT NULL,
    paid integer NOT NULL,
    caid integer NOT NULL,
    pavid integer NOT NULL,
    cavid integer NOT NULL
);


ALTER TABLE public.relationship OWNER TO "ibm-cloud-base-user";


ALTER TABLE public.relationship ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.relationship_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);



ALTER TABLE ONLY public.asset_bookmarks
    ADD CONSTRAINT asset_bookmarks_pk PRIMARY KEY (asset_id, email);



ALTER TABLE ONLY public.actions
    ADD CONSTRAINT asset_click_thrus_pkey PRIMARY KEY (action_id);



ALTER TABLE ONLY public.asset_collection
    ADD CONSTRAINT asset_collection_pkey PRIMARY KEY (asset_id, collection_id);



ALTER TABLE ONLY public.collections
    ADD CONSTRAINT asset_collections_collection_name_key UNIQUE (collection_name);



ALTER TABLE ONLY public.collections
    ADD CONSTRAINT asset_collections_pkey PRIMARY KEY (collection_id);



ALTER TABLE ONLY public.asset_product
    ADD CONSTRAINT asset_product_pk PRIMARY KEY (asset_id, product_id);



ALTER TABLE ONLY public.asset_product
    ADD CONSTRAINT asset_product_un UNIQUE (asset_id, product_id);



ALTER TABLE ONLY public.asset_ratings
    ADD CONSTRAINT asset_ratings_pk PRIMARY KEY (rating_id);



ALTER TABLE ONLY public.asset_types
    ADD CONSTRAINT asset_types_pkey PRIMARY KEY (type_id);



ALTER TABLE ONLY public.asset_types
    ADD CONSTRAINT asset_types_type_name_key UNIQUE (type_name);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_asset_name_key UNIQUE (asset_name);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_pkey PRIMARY KEY (asset_id);



ALTER TABLE ONLY public.attributes
    ADD CONSTRAINT attributes_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.attributevalues
    ADD CONSTRAINT attributevalues_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.brands
    ADD CONSTRAINT brand_pkey PRIMARY KEY (brand_id);



ALTER TABLE ONLY public.brands
    ADD CONSTRAINT brands_brand_name_key UNIQUE (brand_name);



ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comment_pk PRIMARY KEY (comment_id);



ALTER TABLE ONLY public.entities
    ADD CONSTRAINT entities_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_pkey PRIMARY KEY (eid, aid, vid);



ALTER TABLE ONLY public.nominations
    ADD CONSTRAINT nominations_pkey PRIMARY KEY (nomination_id);



ALTER TABLE ONLY public.offering_types
    ADD CONSTRAINT offering_types_pk PRIMARY KEY (offering_type_id);



ALTER TABLE ONLY public.offering_types
    ADD CONSTRAINT offering_types_un UNIQUE (offering_type_id, offering_type_name);



ALTER TABLE ONLY public.parent
    ADD CONSTRAINT parent_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.practices
    ADD CONSTRAINT practices_pkey PRIMARY KEY (practice_id);



ALTER TABLE ONLY public.practices
    ADD CONSTRAINT practices_practice_name_key UNIQUE (practice_name);



ALTER TABLE ONLY public.products
    ADD CONSTRAINT products_pkey PRIMARY KEY (product_id);



ALTER TABLE ONLY public.relationship
    ADD CONSTRAINT relationship_pkey PRIMARY KEY (id);



CREATE INDEX fki_asset_asset_bookmakr_fk ON public.asset_bookmarks USING btree (asset_id);



CREATE INDEX fki_clicks_asset_fk ON public.actions USING btree (related_asset);



CREATE INDEX fki_nomination_asset_fk ON public.nominations USING btree (asset_id);



CREATE TRIGGER set_public_asset_collections_updated_at BEFORE UPDATE ON public.collections FOR EACH ROW EXECUTE FUNCTION public.set_current_timestamp_updated_at();



COMMENT ON TRIGGER set_public_asset_collections_updated_at ON public.collections IS 'trigger to set value of column "updated_at" to current timestamp on row update';



CREATE TRIGGER set_public_assets_updated_at BEFORE UPDATE ON public.assets FOR EACH ROW EXECUTE FUNCTION public.set_current_timestamp_updated_at();



COMMENT ON TRIGGER set_public_assets_updated_at ON public.assets IS 'trigger to set value of column "updated_at" to current timestamp on row update';



CREATE TRIGGER set_public_comment_updated_at BEFORE UPDATE ON public.comments FOR EACH ROW EXECUTE FUNCTION public.set_current_timestamp_updated_at();



ALTER TABLE ONLY public.asset_bookmarks
    ADD CONSTRAINT asset_asset_bookmakr_fk FOREIGN KEY (asset_id) REFERENCES public.assets(asset_id) ON DELETE CASCADE NOT VALID;



ALTER TABLE ONLY public.asset_collection
    ADD CONSTRAINT asset_collection_asset_fk FOREIGN KEY (asset_id) REFERENCES public.assets(asset_id) ON DELETE CASCADE;



ALTER TABLE ONLY public.asset_collection
    ADD CONSTRAINT asset_collection_collection_fk FOREIGN KEY (collection_id) REFERENCES public.collections(collection_id) ON DELETE CASCADE;



ALTER TABLE ONLY public.asset_product
    ADD CONSTRAINT asset_product_asset_fk FOREIGN KEY (asset_id) REFERENCES public.assets(asset_id) ON DELETE CASCADE;



ALTER TABLE ONLY public.asset_product
    ADD CONSTRAINT asset_product_product_fk FOREIGN KEY (product_id) REFERENCES public.products(product_id) ON DELETE CASCADE;



ALTER TABLE ONLY public.asset_ratings
    ADD CONSTRAINT asset_ratings_assets_fk FOREIGN KEY (related_asset) REFERENCES public.assets(asset_id) ON DELETE CASCADE;



ALTER TABLE ONLY public.asset_ratings
    ADD CONSTRAINT asset_ratings_fk FOREIGN KEY (related_asset) REFERENCES public.assets(asset_id) ON UPDATE CASCADE ON DELETE CASCADE;



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_asset_type_fkey FOREIGN KEY (asset_type) REFERENCES public.asset_types(type_id);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_brand_fk FOREIGN KEY (asset_brand) REFERENCES public.brands(brand_id);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_offering_type_fk FOREIGN KEY (asset_offering_type) REFERENCES public.offering_types(offering_type_id);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_practice_fk FOREIGN KEY (asset_practice) REFERENCES public.practices(practice_id);



ALTER TABLE ONLY public.attributevalues
    ADD CONSTRAINT attributevalues_attributes_fk FOREIGN KEY (aid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.actions
    ADD CONSTRAINT clicks_asset_fk FOREIGN KEY (related_asset) REFERENCES public.assets(asset_id) ON DELETE CASCADE NOT VALID;



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_attributes_fk FOREIGN KEY (aid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_attributevalues_fk FOREIGN KEY (vid) REFERENCES public.attributevalues(id);



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_entities_fk FOREIGN KEY (eid) REFERENCES public.assets(asset_id);



ALTER TABLE ONLY public.nominations
    ADD CONSTRAINT nomination_asset_fk FOREIGN KEY (asset_id) REFERENCES public.assets(asset_id) ON DELETE CASCADE NOT VALID;



ALTER TABLE ONLY public.parent
    ADD CONSTRAINT parent_attributes_fk FOREIGN KEY (paid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.parent
    ADD CONSTRAINT parent_attributes_fk_1 FOREIGN KEY (caid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.practices
    ADD CONSTRAINT practices_owning_brand_fkey FOREIGN KEY (owning_brand) REFERENCES public.brands(brand_id) ON UPDATE CASCADE ON DELETE CASCADE;



ALTER TABLE ONLY public.relationship
    ADD CONSTRAINT relationship_attributes_fk FOREIGN KEY (paid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.relationship
    ADD CONSTRAINT relationship_attributes_fk_1 FOREIGN KEY (caid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.relationship
    ADD CONSTRAINT relationship_attributevalues_fk FOREIGN KEY (pavid) REFERENCES public.attributevalues(id);



ALTER TABLE ONLY public.relationship
    ADD CONSTRAINT relationship_attributevalues_fk_1 FOREIGN KEY (cavid) REFERENCES public.attributevalues(id);



GRANT USAGE ON SCHEMA ibm_extension TO PUBLIC;



REVOKE ALL ON FUNCTION public.create_dblink_extension() FROM PUBLIC;
GRANT ALL ON FUNCTION public.create_dblink_extension() TO admin;



REVOKE ALL ON FUNCTION public.create_subscription(subscription_name text, host_ip text, portnum text, password text, username text, db_name text, publisher_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.create_subscription(subscription_name text, host_ip text, portnum text, password text, username text, db_name text, publisher_name text) TO admin;



REVOKE ALL ON FUNCTION public.delete_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.delete_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.disable_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.disable_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.enable_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.enable_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.kill_all_connections() FROM PUBLIC;
GRANT ALL ON FUNCTION public.kill_all_connections() TO admin;



REVOKE ALL ON FUNCTION public.list_subscriptions() FROM PUBLIC;
GRANT ALL ON FUNCTION public.list_subscriptions() TO admin;



REVOKE ALL ON FUNCTION public.pg_kill_connection(integer) FROM PUBLIC;
GRANT ALL ON FUNCTION public.pg_kill_connection(integer) TO admin;



REVOKE ALL ON FUNCTION public.refresh_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.refresh_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.set_pgaudit_session_logging(events text[]) FROM PUBLIC;
GRANT ALL ON FUNCTION public.set_pgaudit_session_logging(events text[]) TO admin;



REVOKE ALL ON FUNCTION public.subscription_slot_none(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.subscription_slot_none(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.update_to_postgis_25() FROM PUBLIC;
GRANT ALL ON FUNCTION public.update_to_postgis_25() TO admin;



REVOKE ALL ON FUNCTION public.update_to_postgis_31() FROM PUBLIC;
GRANT ALL ON FUNCTION public.update_to_postgis_31() TO admin;



