def main():
    H, E, A, O, W, X = map(int, input().split())

    if (H + E + A + X) >= (O + W):
        print("Middle-earth is safe.")
    else:
        print("Sauron has returned.")


if __name__ == "__main__":
    main()
