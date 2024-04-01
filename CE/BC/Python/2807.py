def main():
    size = int(input())

    sequence = [1 for _ in range(size)]

    for i in range(2, size):
        sequence[i] = sequence[i - 1] + sequence[i - 2]

    print(" ".join([str(x) for x in sequence[::-1]]))


if __name__ == "__main__":
    main()
