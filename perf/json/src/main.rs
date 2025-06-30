use serde::Deserialize;
use std::time::Instant;

#[derive(Debug, Deserialize)]
struct Person {
    name: String,
}

fn main() {
    // The JSON string to parse.
    let json_str = r#"{"name": "Alice"}"#;

    // Start the timer.
    let start = Instant::now();

    for i in 0..10000000    {
    // Parse the JSON string into our Person struct.
    let person: Person =
        serde_json::from_str(json_str).expect("Failed to parse JSON");
    }
    // Stop the timer.
    let duration = start.elapsed();

    // Output the parsed struct and how long the parsing took.
    println!("Parsing took: {:?}", duration);
}

