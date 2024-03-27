def main():
    iter = int(input())

    for _ in range(iter):
        strips, *outlets = map(int, input().split())
        final = 0

        for i in range(strips):
            final += (outlets[i] - 1)

        final += 1
        print(final)


if __name__ == "__main__":
    main()
