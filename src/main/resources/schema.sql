create schema portfolio;

CREATE TABLE portfolio.TENANT(
  ID VARCHAR(255) NOT NULL
) organize by row;

create table portfolio.DUNS(
  NUMBER VARCHAR(255) NOT NULL,
  TENANT_ID VARCHAR(255) NOT NULL,
  name varchar(500)
) organize by row;

ALTER TABLE portfolio.TENANT ADD constraint pk_tenant PRIMARY KEY (ID);
ALTER TABLE portfolio.DUNS ADD constraint pk_number_tenant PRIMARY KEY (NUMBER, TENANT_ID);
ALTER TABLE portfolio.DUNS ADD FOREIGN KEY (TENANT_ID) REFERENCES portfolio.TENANT(ID);

create table portfolio.naics(
  id int not null,
  code int not null,
  title varchar(255) not null,
  description varchar(4000)
) organize by row;

alter table portfolio.naics add constraint pk_naics_id primary key(id);

create table portfolio.naics_embeddings(
id int not null generated always as identity,
chunk_id int not null,
  naics_id int not null,
  embedding vector(768, FLOAT32) not null,
  chunk varchar (4000)
) organize by column;
ALTER TABLE portfolio.naics_embeddings ADD constraint pk_id PRIMARY KEY (id);

