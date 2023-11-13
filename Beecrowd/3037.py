def main():
    iter = int(input())

    for _ in range(iter):
        john, mary = 0, 0
        for _ in range(3):
            d, x = map(int, input().split())
            john += d * x
        for _ in range(3):
            d, x = map(int, input().split())
            mary += d * x

        print("JOAO" if john > mary else "MARIA")


if __name__ == "__main__":
    main()
