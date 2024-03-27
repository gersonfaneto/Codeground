def main():
    iter = int(input())

    cars, dolls = 0, 0

    for _ in range(iter):
        _, gender = input().split()

        if gender == "M":
            cars += 1
        else:
            dolls += 1

    print(f"{cars} carrinhos")
    print(f"{dolls} bonecas")


if __name__ == "__main__":
    main()
