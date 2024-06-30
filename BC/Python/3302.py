def main():
    iter = int(input())
    numbers = []

    for _ in range(iter):
        numbers.append(int(input()))

    for i in range(iter):
        print(f"resposta {i + 1}: {numbers[i]}")


if __name__ == "__main__":
    main()
