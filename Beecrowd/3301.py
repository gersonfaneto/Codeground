def main():
    names = ["huguinho", "zezinho", "luisinho"]
    ages = map(int, input().split())

    pairs = [(x, y) for x, y in zip(names, ages)]

    pairs.sort(key=lambda x: x[1])

    print(pairs[1][0])


if __name__ == "__main__":
    main()
