def main():
    nFruits, nLines = map(int, input().split())

    allFruits = []
    allLines = []

    for _ in range(nFruits):
        allFruits.append(input().lower())

    for _ in range(nLines):
        allLines.append(input().lower())

    for fruit in allFruits:
        for line in allLines:
            if fruit in line or fruit[::-1] in line:
                print(f"Sheldon come a fruta {fruit}")
                break
        else:
            print(f"Sheldon detesta a fruta {fruit}")


if __name__ == "__main__":
    main()
