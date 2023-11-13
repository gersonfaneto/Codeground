def main():
    balls = int(input())
    branches = int(input())

    branches = branches - 1 if branches % 2 != 0 else branches

    missing = (branches // 2) - balls

    if (missing > 0):
        print(f"Faltam {missing} bolinha(s)")
    else:
        print("Amelia tem todas bolinhas!")


if __name__ == "__main__":
    main()
