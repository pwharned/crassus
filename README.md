# Crassus

A fully contained, self documenting framework for developing  REST and RPC APIs in scala3. 


```
sbt caseClassGenerator/publishLocal
```


# SQL to Case Class Generator

A Scala code generator that converts SQL schema files into Scala case classes, with support for Higher-Kinded Data (HKD) patterns.

## Overview

This generator parses SQL DDL statements and automatically creates corresponding Scala case classes. It's particularly useful for:
- Type-safe database modeling
- Generating boilerplate-free data classes from existing schemas
- Supporting functional programming patterns with HKD

## Features

- **SQL Schema Parsing**: Reads standard SQL `CREATE TABLE` and `ALTER TABLE` statements
- **Primary Key Detection**: Automatically identifies and marks primary key columns
- **Generated Column Support**: Handles `GENERATED ALWAYS AS IDENTITY` columns
- **HKD Support**: Generates Higher-Kinded Data classes for advanced type-level programming
- **Nullable Column Handling**: Properly maps SQL nullable columns to Scala `Option` types

## Setup

### Add to your `project/plugins.sbt`:
```scala
addSbtPlugin("org.pwharned" % "sql-case-class-generator" % "x.x.x")
```

### Enable in your `build.sbt`:
```scala
enablePlugins(SqlCaseClassGeneratorPlugin)
```

## Quick Start

### 1. Create a SQL schema file

Create `schema.sql`:
```sql
CREATE TABLE users (
    id INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE users ADD PRIMARY KEY (id);
ALTER TABLE users ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY;
```

### 2. Generate case classes

```scala
import org.pwharned.generator.CaseClassGenerator

// Generate HKD case classes (default)
val hkdClasses = CaseClassGenerator.generateCaseClasses("schema.sql")

// Generate regular case classes
val regularClasses = CaseClassGenerator.generateCaseClasses("schema.sql", hkd = false)
```

### 3. Output

#### HKD Case Classes (default):
```scala
case class Users[F[_]](
  id: F[PrimaryKey[Int]],
  name: F[String],
  email: F[Option[String]],
  created_at: F[Option[Timestamp]]
)
```

#### Regular Case Classes:
```scala
case class Users(
  id: Int,
  name: String,  
  email: Option[String],
  created_at: Option[Timestamp]
)
```

## API Reference

### `generateCaseClasses`

```scala
def generateCaseClasses(filePath: String, hkd: Boolean = true): String
```

**Parameters:**
- `filePath`: Path to your SQL schema file
- `hkd`: Whether to generate Higher-Kinded Data classes (default: `true`)

**Returns:** Generated Scala case class definitions as a `String`

**Throws:** `IllegalArgumentException` if the schema file doesn't exist or is invalid

## Supported SQL Features

### CREATE TABLE
```sql
CREATE TABLE example (
    id INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    price DECIMAL(10,2),
    active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### ALTER TABLE - Primary Keys
```sql
ALTER TABLE example ADD PRIMARY KEY (id);
```

### ALTER TABLE - Generated Columns
```sql
ALTER TABLE example ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY;
```

## Type Mappings

| SQL Type | Scala Type | Notes |
|----------|------------|-------|
| `INT` | `Int` | |
| `VARCHAR(n)` | `String` | |
| `TEXT` | `String` | |
| `TIMESTAMP` | `Timestamp` | |
| `BOOLEAN` | `Boolean` | |
| `DECIMAL(p,s)` | `BigDecimal` | |

### Special Handling

- **Nullable columns**: `NULL` → `Option[T]`, `NOT NULL` → `T`
- **Primary keys**: Wrapped in `PrimaryKey[T]` for type safety
- **Generated columns**: Automatically detected from `GENERATED ALWAYS AS IDENTITY`

## Integration Examples

### SBT Task Integration

Add to your `build.sbt`:

```scala
lazy val generateModels = taskKey[Unit]("Generate case classes from SQL schema")

generateModels := {
  val output = CaseClassGenerator.generateCaseClasses("src/main/resources/schema.sql")
  val finalOutput = s"""
    |package com.myapp.models
    |
    |import java.sql.Timestamp
    |
    |$output
    |""".stripMargin
    
  IO.write(file("src/main/scala/com/myapp/models/Generated.scala"), finalOutput)
  println("✅ Generated case classes successfully!")
}
```

Run with:
```bash
sbt generateModels
```

### Database Migration Integration

```scala
lazy val generateAfterMigrate = taskKey[Unit]("Generate models after DB migration")

generateAfterMigrate := {
  // Run your migration tool first
  (Compile / runMain).toTask(" com.myapp.RunMigrations").value
  
  // Then generate case classes
  val models = CaseClassGenerator.generateCaseClasses("migrations/latest-schema.sql")
  val output = s"""
    |package com.myapp.models
    |
    |import java.sql.Timestamp
    |
    |$models
    |""".stripMargin
    
  IO.write(file("src/main/scala/com/myapp/models/Generated.scala"), output)
}
```

### Continuous Integration

```yaml
# .github/workflows/generate-models.yml
name: Generate Models
on: [push, pull_request]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: coursier/setup-action@v1
        with:
          jvm: adoptium:11
      - name: Generate case classes
        run: sbt generateModels
      - name: Check for changes
        run: git diff --exit-code src/main/scala/com/myapp/models/Generated.scala
```

## Advanced Usage

### Custom Package and Imports

```scala
def generateWithPackage(schemaFile: String, packageName: String): String = {
  val classes = CaseClassGenerator.generateCaseClasses(schemaFile)
  s"""
  |package $packageName
  |
  |import java.sql.Timestamp
  |import java.time.LocalDateTime
  |
  |$classes
  |""".stripMargin
}
```

### Multiple Schema Files

```scala
val schemas = List("users.sql", "products.sql", "orders.sql")
val allClasses = schemas.map(CaseClassGenerator.generateCaseClasses(_)).mkString("\n\n")
```

## Error Handling

The generator provides clear error messages:

```scala
try {
  val classes = CaseClassGenerator.generateCaseClasses("nonexistent.sql")
} catch {
  case e: IllegalArgumentException => 
    println(s" Error: ${e.getMessage}")
    // Output: Schema file not found at: nonexistent.sql
}
```

## Limitations

- Only supports a subset of SQL DDL statements
- Complex constraints (CHECK, UNIQUE) are not reflected in generated classes
- Foreign key relationships are not automatically resolved
- Custom/user-defined types may need manual mapping
- Does not handle table inheritance or advanced PostgreSQL features

## Examples

### E-commerce Schema

```sql
-- schema.sql
CREATE TABLE products (
    id INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    price DECIMAL(10,2) NOT NULL,
    stock_quantity INT DEFAULT 0,
    active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP
);

CREATE TABLE orders (
    id INT NOT NULL,
    product_id INT NOT NULL,
    quantity INT NOT NULL,
    total_price DECIMAL(10,2) NOT NULL,
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE products ADD PRIMARY KEY (id);
ALTER TABLE orders ADD PRIMARY KEY (id);
ALTER TABLE products ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY;
ALTER TABLE orders ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY;
```

**Generated Output:**
```scala
case class Products[F[_]](
  id: F[PrimaryKey[Int]],
  name: F[String],
  description: F[Option[String]],
  price: F[BigDecimal],
  stock_quantity: F[Option[Int]],
  active: F[Option[Boolean]],
  created_at: F[Option[Timestamp]],
  updated_at: F[Option[Timestamp]]
)

case class Orders[F[_]](
  id: F[PrimaryKey[Int]],
  product_id: F[Int],
  quantity: F[Int],
  total_price: F[BigDecimal],
  order_date: F[Option[Timestamp]]
)
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new SQL features
4. Update documentation
5. Submit a pull request
