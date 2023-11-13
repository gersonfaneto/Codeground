def main():
    start, obtained, cost = map(int, input().split())
    result = 0
    
    total = start + obtained

    while total > 0:
        total -= cost
        total += 1

        if total > 0:
            result += 1

    print(result)


if __name__ == "__main__":
    main()
