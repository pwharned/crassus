

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


CREATE SCHEMA ibm_extension;


ALTER SCHEMA ibm_extension OWNER TO ibm;


CREATE SCHEMA tiger;


ALTER SCHEMA tiger OWNER TO ibm;


CREATE SCHEMA tiger_data;


ALTER SCHEMA tiger_data OWNER TO ibm;


CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA ibm_extension;



COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';



CREATE EXTENSION IF NOT EXISTS vector WITH SCHEMA ibm_extension;



COMMENT ON EXTENSION vector IS 'vector data type and ivfflat and hnsw access methods';



CREATE FUNCTION public.create_dblink_extension() RETURNS boolean
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
        DECLARE
                persist_dblink_extension boolean;
        BEGIN
            SELECT EXISTS(SELECT 1 FROM pg_extension WHERE extname='dblink')::boolean::boolean INTO persist_dblink_extension;
            IF persist_dblink_extension = FALSE
            THEN
                CREATE EXTENSION IF NOT EXISTS dblink;
            END IF;
            RETURN persist_dblink_extension;
        END
$$;


ALTER FUNCTION public.create_dblink_extension() OWNER TO ibm;


CREATE FUNCTION public.create_subscription(subscription_name text, host_ip text, portnum text, password text, username text, db_name text, publisher_name text, copy_data boolean DEFAULT true, origin text DEFAULT 'ANY'::text, failover boolean DEFAULT false) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                DECLARE
                     persist_dblink_extension boolean;
                     pg_ver int;
                BEGIN
                    IF origin is distinct from 'NONE' AND origin is distinct from 'ANY'
                    THEN
                        RETURN 'invalid argument value for origin';
                    END IF;
                    persist_dblink_extension := create_dblink_extension();
                    PERFORM dblink_connect(format('dbname=%I', db_name));
                    -- Support origin and copy_data only in pg17 (earlier version don't support origin)
                    SELECT current_setting('server_version_num') INTO pg_ver;
                    IF pg_ver < 170000
                    THEN
                        -- PG < 17 (copy_data was supported earlier, but we care about origin)
                        PERFORM dblink_exec(format('CREATE SUBSCRIPTION %I CONNECTION %L PUBLICATION %I',
                            subscription_name,
                            format('host=%L port=%L password=%L user=%L dbname=%I sslmode=require', host_ip, portNum, password, username, db_name),
                            publisher_name));
                    ELSE
                        -- PG 17 and newer
                        PERFORM dblink_exec(format('CREATE SUBSCRIPTION %I CONNECTION %L PUBLICATION %I WITH ( copy_data = %s , failover = %s , origin = %s)',
                            subscription_name,
                            format('host=%L port=%L password=%L user=%L dbname=%I sslmode=require', host_ip, portNum, password, username, db_name),
                            publisher_name, copy_data::text, failover::text, origin));
                    END IF;
                    PERFORM dblink_disconnect();
                    IF persist_dblink_extension = FALSE
                    THEN
                        DROP EXTENSION IF EXISTS dblink;
                    END IF;
                    RETURN 'ok';
                END
        $$;


ALTER FUNCTION public.create_subscription(subscription_name text, host_ip text, portnum text, password text, username text, db_name text, publisher_name text, copy_data boolean, origin text, failover boolean) OWNER TO ibm;


CREATE FUNCTION public.delete_subscription(subscription_name text, db_name text) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                DECLARE
                     persist_dblink_extension boolean;
                BEGIN
                    persist_dblink_extension := create_dblink_extension();
                    PERFORM dblink_connect(format('dbname=%I', db_name));
                    PERFORM dblink_exec(format('Drop SUBSCRIPTION %I', subscription_name));
                    PERFORM dblink_disconnect();

                    IF persist_dblink_extension = FALSE
                    THEN
                        DROP EXTENSION IF EXISTS dblink;
                    END IF;
                    RETURN 'ok';
                END
        $$;


ALTER FUNCTION public.delete_subscription(subscription_name text, db_name text) OWNER TO ibm;


CREATE FUNCTION public.disable_subscription(subscription_name text, db_name text) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                DECLARE
                     persist_dblink_extension boolean;
                BEGIN
                    persist_dblink_extension := create_dblink_extension();
                    PERFORM dblink_connect(format('dbname=%I', db_name));
                    PERFORM dblink_exec(format('ALTER SUBSCRIPTION %I DISABLE', subscription_name));
                    PERFORM dblink_disconnect();
                    IF persist_dblink_extension = FALSE
                    THEN
                        DROP EXTENSION IF EXISTS dblink;
                    END IF;
                    RETURN 'ok';
                END
        $$;


ALTER FUNCTION public.disable_subscription(subscription_name text, db_name text) OWNER TO ibm;


CREATE FUNCTION public.enable_subscription(subscription_name text, db_name text) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                DECLARE
                     persist_dblink_extension boolean;
                BEGIN
                    persist_dblink_extension := create_dblink_extension();
                    PERFORM dblink_connect(format('dbname=%I', db_name));
                    PERFORM dblink_exec(format('ALTER SUBSCRIPTION %I ENABLE', subscription_name));
                    PERFORM dblink_disconnect();
                    IF persist_dblink_extension = FALSE
                    THEN
                        DROP EXTENSION IF EXISTS dblink;
                    END IF;
                    RETURN 'ok';
                END
        $$;


ALTER FUNCTION public.enable_subscription(subscription_name text, db_name text) OWNER TO ibm;


CREATE FUNCTION public.grant_admin_option_to_roles(VARIADIC role_list text[]) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
    role TEXT;
    pg_version INT;
BEGIN
    -- Get major PostgreSQL version
    SELECT current_setting('server_version_num')::INT / 10000 INTO pg_version;

    -- Check if version is 16 or higher
    IF pg_version < 16 THEN
        RAISE NOTICE 'Skipping: This function only applies to PostgreSQL 16 or higher. Current version: %', pg_version;
        RETURN;
    END IF;

    -- Loop through roles and grant ADMIN OPTION to admin (excluding certain roles)
    FOREACH role IN ARRAY role_list LOOP
        role := trim(role); -- Trim spaces from each role name

        -- Exclude specific roles dynamically
        IF role IN (
            'ibm', 
            'admin', 
            'repl', 
            'ibm-rewind', 
            'ibm-replication', 
            'ibm-cloud-base-user-ro', 
            'pg_monitor', 
            'pg_signal_backend'
        ) THEN
            RAISE NOTICE 'Skipping role: % (not allowed)', role;
        ELSE
            EXECUTE format('GRANT %I TO admin WITH ADMIN OPTION', role);
            RAISE NOTICE 'Granted ADMIN OPTION to admin for role: %', role;
            RAISE NOTICE 'Successfully processed roles.';
        END IF;
    END LOOP;
END;
$$;


ALTER FUNCTION public.grant_admin_option_to_roles(VARIADIC role_list text[]) OWNER TO ibm;


CREATE FUNCTION public.kill_all_connections() RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
        BEGIN
                EXECUTE 'SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE pg_stat_activity.datname = current_database() AND pid <> pg_backend_pid()';
                RETURN 'ok';
        END
$$;


ALTER FUNCTION public.kill_all_connections() OWNER TO ibm;


CREATE FUNCTION public.list_subscriptions() RETURNS TABLE(subdbid oid, subname name, subowner oid, subenabled boolean, subconninfo text, subslotname name, subsynccommit text, subpublications text[])
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                BEGIN
                    RETURN QUERY SELECT * from pg_subscription;
                END
        $$;


ALTER FUNCTION public.list_subscriptions() OWNER TO ibm;


CREATE FUNCTION public.pg_kill_connection(integer) RETURNS boolean
    LANGUAGE sql SECURITY DEFINER
    AS $_$select pg_terminate_backend($1);$_$;


ALTER FUNCTION public.pg_kill_connection(integer) OWNER TO ibm;


CREATE FUNCTION public.refresh_subscription(subscription_name text, db_name text) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                DECLARE
                     persist_dblink_extension boolean;
                BEGIN
                    persist_dblink_extension := create_dblink_extension();
                    PERFORM dblink_connect(format('dbname=%I', db_name));
                    PERFORM dblink_exec(format('ALTER SUBSCRIPTION %I REFRESH PUBLICATION', subscription_name));
                    PERFORM dblink_disconnect();

                    IF persist_dblink_extension = FALSE
                    THEN
                        DROP EXTENSION IF EXISTS dblink;
                    END IF;
                    RETURN 'ok';
                END
        $$;


ALTER FUNCTION public.refresh_subscription(subscription_name text, db_name text) OWNER TO ibm;


CREATE FUNCTION public.set_current_timestamp_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  _new record;
BEGIN
  _new := NEW;
  _new."updated_at" = NOW();
  RETURN _new;
END;
$$;


ALTER FUNCTION public.set_current_timestamp_updated_at() OWNER TO "ibm-cloud-base-user";


CREATE FUNCTION public.set_pgaudit_session_logging(events text[]) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
        DECLARE
                persist_dblink_extension boolean;
                allowed_events TEXT [];
                input_valid boolean;
                pgaudit_ver_check boolean;
        BEGIN
            --misc_set is only available in pgAudit >= 1.4
            SELECT version >= '1.4' FROM pg_available_extension_versions WHERE name = 'pgaudit' INTO pgaudit_ver_check;
            IF pgaudit_ver_check is true
            THEN
                allowed_events := '{function,role,ddl,misc,misc_set,none}';
            ELSE
                allowed_events := '{function,role,ddl,misc,none}';
            END IF;
             --lower case the input for case insensitive comparison.
            SELECT array_agg(x.events) INTO events FROM (SELECT btrim(lower(unnest(events)))::TEXT AS events) AS x;
            SELECT events  <@ allowed_events INTO input_valid;
            IF not input_valid
            THEN
                RAISE EXCEPTION 'Invalid options provided: %, Allowed events: %.', events,allowed_events;
            END IF;
            persist_dblink_extension := create_dblink_extension();
            PERFORM dblink_connect(format('dbname=postgres'));
            PERFORM dblink_exec(format('ALTER SYSTEM SET pgaudit.log TO %L;', array_to_string(events, ',')));
            PERFORM dblink_disconnect();
            PERFORM pg_reload_conf();
            IF persist_dblink_extension = FALSE
            THEN
                DROP EXTENSION IF EXISTS dblink;
            END IF;
            RETURN 'ok';
        END
$$;


ALTER FUNCTION public.set_pgaudit_session_logging(events text[]) OWNER TO ibm;


CREATE FUNCTION public.subscription_slot_none(subscription_name text, db_name text) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
                DECLARE
                     persist_dblink_extension boolean;
                BEGIN
                    persist_dblink_extension := create_dblink_extension();
                    PERFORM dblink_connect(format('dbname=%I', db_name));
                    PERFORM dblink_exec(format('ALTER SUBSCRIPTION %I SET (slot_name = NONE)', subscription_name));
                    PERFORM dblink_disconnect();

                    IF persist_dblink_extension = FALSE
                    THEN
                        DROP EXTENSION IF EXISTS dblink;
                    END IF;
                    RETURN 'ok';
                END
        $$;


ALTER FUNCTION public.subscription_slot_none(subscription_name text, db_name text) OWNER TO ibm;


CREATE FUNCTION public.update_to_postgis_25() RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
        DECLARE
            postgis_25_ver text;
        BEGIN
                IF NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname='postgis') THEN
                    RAISE EXCEPTION 'PostGIS is not installed.';
                END IF;
                SELECT
                    version
                INTO
                    postgis_25_ver
                FROM
                    pg_available_extension_versions
                WHERE
                    name='postgis' AND
                    version like '2.5%' AND
                    version NOT LIKE '%next';
                EXECUTE 'ALTER EXTENSION postgis UPDATE to '|| quote_literal(postgis_25_ver);
              --  RETURN 'ok';
        END
$$;


ALTER FUNCTION public.update_to_postgis_25() OWNER TO ibm;


CREATE FUNCTION public.update_to_postgis_31() RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
        DECLARE
            postgis_31_ver text;
        BEGIN
                IF NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname='postgis') THEN
                    RAISE EXCEPTION 'PostGIS is not installed.';
                END IF;
                SELECT
                    version
                INTO
                    postgis_31_ver
                FROM
                    pg_available_extension_versions
                WHERE
                    name='postgis' AND
                    version like '3.1%' AND
                    version NOT LIKE '%next';
                EXECUTE 'ALTER EXTENSION postgis UPDATE to '|| quote_literal(postgis_31_ver);
                EXECUTE 'SELECT postgis_extensions_upgrade();';
                EXECUTE 'DROP EXTENSION postgis_raster;';
              --  RETURN 'ok';
        END
$$;


ALTER FUNCTION public.update_to_postgis_31() OWNER TO ibm;

SET default_tablespace = '';

SET default_table_access_method = heap;


CREATE TABLE public.actions (
    action_id uuid DEFAULT gen_random_uuid() NOT NULL,
    action_user text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    related_asset uuid NOT NULL,
    action_type text NOT NULL
);


ALTER TABLE public.actions OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.asset_bookmarks (
    asset_id uuid NOT NULL,
    email text NOT NULL
);


ALTER TABLE public.asset_bookmarks OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.asset_collection (
    asset_id uuid NOT NULL,
    collection_id uuid NOT NULL
);


ALTER TABLE public.asset_collection OWNER TO "ibm-cloud-base-user";


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
    asset_market text,
    asset_type text
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


CREATE VIEW public.duplicates AS
 WITH a AS (
         SELECT count(*) AS count,
            attributevalues.value
           FROM public.attributevalues
          GROUP BY attributevalues.value, attributevalues.aid
         HAVING (count(*) > 1)
        ), b AS (
         SELECT av.value,
            av.aid,
            av.id,
            row_number() OVER (PARTITION BY av.value) AS rowid
           FROM (a
             JOIN public.attributevalues av ON (((av.value)::text = (a.value)::text)))
        ), c AS (
         SELECT b.value,
            b.id,
            b.aid,
            b.rowid
           FROM b
        )
 SELECT c.value,
    c.id,
    c.aid,
    cc.id AS newid,
    c.rowid
   FROM (c
     LEFT JOIN ( SELECT c_1.value,
            c_1.id,
            c_1.aid,
            c_1.rowid
           FROM c c_1
          WHERE (c_1.rowid = 1)) cc ON (((cc.value)::text = (c.value)::text)));


ALTER VIEW public.duplicates OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.embeddings (
    embedding_id integer NOT NULL,
    asset_id uuid NOT NULL,
    embedding_vector ibm_extension.vector(768)
);


ALTER TABLE public.embeddings OWNER TO "ibm-cloud-base-user";


CREATE SEQUENCE public.embeddings_embedding_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.embeddings_embedding_id_seq OWNER TO "ibm-cloud-base-user";


ALTER SEQUENCE public.embeddings_embedding_id_seq OWNED BY public.embeddings.embedding_id;



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


CREATE TABLE public.geos (
    parent text,
    child text
);


ALTER TABLE public.geos OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.mappings (
    parent text,
    child text
);


ALTER TABLE public.mappings OWNER TO "ibm-cloud-base-user";


CREATE TABLE public.new_practices (
    original text,
    new text
);


ALTER TABLE public.new_practices OWNER TO "ibm-cloud-base-user";


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



CREATE TABLE public.test (
    id integer,
    id2 integer
);


ALTER TABLE public.test OWNER TO "ibm-cloud-base-user";


ALTER TABLE ONLY public.embeddings ALTER COLUMN embedding_id SET DEFAULT nextval('public.embeddings_embedding_id_seq'::regclass);



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



ALTER TABLE ONLY public.embeddings
    ADD CONSTRAINT embeddings_pkey PRIMARY KEY (embedding_id);



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
    ADD CONSTRAINT assets_brand_fk FOREIGN KEY (asset_brand) REFERENCES public.brands(brand_id);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_offering_type_fk FOREIGN KEY (asset_offering_type) REFERENCES public.offering_types(offering_type_id);



ALTER TABLE ONLY public.assets
    ADD CONSTRAINT assets_practice_fk FOREIGN KEY (asset_practice) REFERENCES public.practices(practice_id);



ALTER TABLE ONLY public.attributevalues
    ADD CONSTRAINT attributevalues_attributes_fk FOREIGN KEY (aid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.actions
    ADD CONSTRAINT clicks_asset_fk FOREIGN KEY (related_asset) REFERENCES public.assets(asset_id) ON DELETE CASCADE NOT VALID;



ALTER TABLE ONLY public.embeddings
    ADD CONSTRAINT embeddings_asset_id_fkey FOREIGN KEY (asset_id) REFERENCES public.assets(asset_id) ON DELETE CASCADE;



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_attributes_fk FOREIGN KEY (aid) REFERENCES public.attributes(id);



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_attributevalues_fk FOREIGN KEY (vid) REFERENCES public.attributevalues(id);



ALTER TABLE ONLY public.entityattributes
    ADD CONSTRAINT entityattributes_entities_fk FOREIGN KEY (eid) REFERENCES public.assets(asset_id) ON DELETE CASCADE;



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



REVOKE ALL ON FUNCTION public.create_subscription(subscription_name text, host_ip text, portnum text, password text, username text, db_name text, publisher_name text, copy_data boolean, origin text, failover boolean) FROM PUBLIC;
GRANT ALL ON FUNCTION public.create_subscription(subscription_name text, host_ip text, portnum text, password text, username text, db_name text, publisher_name text, copy_data boolean, origin text, failover boolean) TO admin;



REVOKE ALL ON FUNCTION public.delete_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.delete_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.disable_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.disable_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.enable_subscription(subscription_name text, db_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.enable_subscription(subscription_name text, db_name text) TO admin;



REVOKE ALL ON FUNCTION public.grant_admin_option_to_roles(VARIADIC role_list text[]) FROM PUBLIC;
GRANT ALL ON FUNCTION public.grant_admin_option_to_roles(VARIADIC role_list text[]) TO admin;



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



