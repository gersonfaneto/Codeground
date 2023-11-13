def main():
    dwarves, distance = map(int, input().split())

    print("{time:.2f}".format(time = distance / (dwarves + 2)))


if __name__ == "__main__":
    main()
