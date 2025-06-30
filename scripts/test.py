#!/usr/bin/env python3
import asyncio
import aiohttp
import time
import argparse
from datetime import datetime

async def make_request(session, url, request_id):
    start_time = time.time()
    try:
        async with session.get(url) as response:
            await response.read()
            status = response.status
            js = await response.text()
            end_time = time.time()
            return {
                "id": request_id,
                "status": status,
                "time": end_time - start_time,
                "success": 200 <= status < 300
            }
    except Exception as e:
        end_time = time.time()
        return {
            "id": request_id,
            "status": None,
            "time": end_time - start_time,
            "success": False,
            "error": str(e)
        }

async def test_concurrency(url, concurrency, total_requests):
    print(f"Testing {url} with {concurrency} concurrent connections")
    print(f"Total requests: {total_requests}")
    print(f"Started at: {datetime.now().strftime('%H:%M:%S')}")
    
    # Create a connection pool with the specified concurrency limit
    connector = aiohttp.TCPConnector(limit=concurrency)
    async with aiohttp.ClientSession(connector=connector) as session:
        tasks = [make_request(session, url, i) for i in range(total_requests)]
        
        # Wait for all requests to complete
        start_time = time.time()
        results = await asyncio.gather(*tasks)
        end_time = time.time()
    
    # Calculate statistics
    successful = [r for r in results if r["success"]]
    failed = [r for r in results if not r["success"]]
    
    if successful:
        avg_time = sum(r["time"] for r in successful) / len(successful)
        max_time = max(r["time"] for r in successful)
        min_time = min(r["time"] for r in successful)
    else:
        avg_time = max_time = min_time = 0
    
    total_time = end_time - start_time
    rps = len(successful) / total_time if total_time > 0 else 0
    
    # Print results
    print(f"\nResults:")
    print(f"Total time: {total_time:.2f} seconds")
    print(f"Successful requests: {len(successful)} ({len(successful)/total_requests*100:.1f}%)")
    print(f"Failed requests: {len(failed)} ({len(failed)/total_requests*100:.1f}%)")
    print(f"Requests per second: {rps:.2f}")
    print(f"Average response time: {avg_time*1000:.2f} ms")
    print(f"Min response time: {min_time*1000:.2f} ms")
    print(f"Max response time: {max_time*1000:.2f} ms")
    
    if failed:
        error_count = {}
        for r in failed:
            error = r.get("error", f"HTTP {r['status']}")
            if error in error_count:
                error_count[error] += 1
            else:
                error_count[error] = 1
        
        print("\nError summary:")
        for error, count in error_count.items():
            print(f"  {error}: {count}")
    
    print(f"Finished at: {datetime.now().strftime('%H:%M:%S')}")
    
    return {
        "concurrency": concurrency,
        "total_requests": total_requests,
        "successful": len(successful),
        "failed": len(failed),
        "total_time": total_time,
        "rps": rps,
        "avg_time": avg_time,
        "min_time": min_time,
        "max_time": max_time
    }

async def run_increasing_concurrency_test(url, start_concurrency, max_concurrency, step, requests_per_test):
    results = []
    
    concurrency = start_concurrency
    while concurrency <= max_concurrency:
        result = await test_concurrency(url, concurrency, requests_per_test)
        results.append(result)
        
        # Wait a bit between tests to let the server recover
        print(f"\nWaiting 3 seconds before next test...\n")
        await asyncio.sleep(3)
        
        concurrency += step
    
    # Print comparison table
    print("\n=== Concurrency Comparison ===")
    print("Concurrency | Success Rate | RPS    | Avg Time (ms)")
    print("------------------------------------------------")
    for r in results:
        success_rate = r["successful"] / r["total_requests"] * 100
        print(f"{r['concurrency']:11} | {success_rate:11.1f}% | {r['rps']:6.2f} | {r['avg_time']*1000:12.2f}")
    
    # Find optimal concurrency
    best_rps = 0
    optimal_concurrency = 0
    for r in results:
        # Only consider tests with high success rate (>95%)
        if r["successful"] / r["total_requests"] >= 0.95 and r["rps"] > best_rps:
            best_rps = r["rps"]
            optimal_concurrency = r["concurrency"]
    
    if optimal_concurrency > 0:
        print(f"\nOptimal concurrency level: {optimal_concurrency} (achieving {best_rps:.2f} RPS)")
    else:
        print("\nNo optimal concurrency level found with >95% success rate")

def main():
    parser = argparse.ArgumentParser(description='Test API concurrency')
    parser.add_argument('--url', required=True, help='URL to test')
    parser.add_argument('--concurrency', type=int, default=10, help='Number of concurrent connections (for single test)')
    parser.add_argument('--requests', type=int, default=100, help='Total number of requests to make')
    parser.add_argument('--find-optimal', action='store_true', help='Find optimal concurrency by testing multiple levels')
    parser.add_argument('--start', type=int, default=5, help='Starting concurrency for optimal test')
    parser.add_argument('--max', type=int, default=100, help='Maximum concurrency for optimal test')
    parser.add_argument('--step', type=int, default=5, help='Step size for concurrency levels')
    
    args = parser.parse_args()
    
    if args.find_optimal:
        asyncio.run(run_increasing_concurrency_test(
            args.url, 
            args.start, 
            args.max, 
            args.step, 
            args.requests
        ))
    else:
        asyncio.run(test_concurrency(args.url, args.concurrency, args.requests))

if __name__ == "__main__":
    main()

