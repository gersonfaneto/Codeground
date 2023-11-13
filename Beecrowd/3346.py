def main():
    a, b = map(float, input().split())
    c = (((1.0 + a / 100.00) * (1.0 + b / 100.00)) - 1.0) * 100.0

    print("{c:.6f}".format(c = c))


if __name__ == "__main__":
    main()
