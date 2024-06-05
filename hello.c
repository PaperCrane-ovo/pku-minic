// Call a func with many params.
// expected output: 2 38 83
// output: 2 22 49

int testParam8(int a0, int a1, int a2, int a3,
        int a4, int a5, int a6, int a7) {
    return a0 - a1 + a2 - a3 - a4 - a5 + a6 + a7;
}

int testParam16(int a0, int a1, int a2, int a3,
        int a4, int a5, int a6, int a7,
        int a8, int a9, int a10, int a11,
        int a12, int a13, int a14, int a15) {
        // putch(10);
        // putint(a0);putch(32);
        // putint(a1);putch(32);
        // putint(a2);putch(32);
        // putint(a3);putch(32);
        // putint(a4);putch(32);
        // putint(a5);putch(32);
        // putint(a6);putch(32);
        // putint(a7);putch(32);
        // putint(a8);putch(32);
        // putint(a9);putch(32);
        // putint(a10);putch(32);
        // putint(a11);putch(32);
        // putint(a12);putch(32);
        // putint(a13);putch(32);
        // putint(a14);putch(32);
        // putint(a15);putch(32);
        // putch(10);
    return a0 + a1 + a2 + a3 - a4 + a5 + a6 + a7 -
            a8 + a9 - a10 + a11 + a12 + a13 + a14 + a15;
}

// int testParam32(int a0, int a1, int a2, int a3,
//         int a4, int a5, int a6, int a7,
//         int a8, int a9, int a10, int a11,
//         int a12, int a13, int a14, int a15,
//         int a16, int a17, int a18, int a19,
//         int a20, int a21, int a22, int a23,
//         int a24, int a25, int a26, int a27,
//         int a28, int a29, int a30, int a31) {
//     return a0 + a1 * a2 + a3 + a4 + a5 + a6 + a7 +
//             a8 + a9 + a10 + a11 - a12 - a13 - a14 - a15 -
//             a16 - a17 - a18 - a19 - a20 - a21 + a22 + a23 +
//             a24 + a25 + a26 + a27 + a28 + a29 + a30 + a31;
// }

int main() {
    int a0;
    int a1;
    int a2;
    int a3;
    int a4;
    int a5;
    int a6;
    int a7;
    int a8;
    int a9;
    int a10;
    int a11;
    int a12;
    int a13;
    int a14;
    int a15;
//     int a16;
//     int a17;
//     int a18;
//     int a19;
//     int a20;
//     int a21;
//     int a22;
//     int a23;
//     int a24;
//     int a25;
//     int a26;
//     int a27;
//     int a28;
//     int a29;
//     int a30;
//     int a31;
    a0 = 0;
    a1 = 1;
    a2 = 2;
    a3 = 3;
    a4 = 4;
    a5 = 5;
    a6 = 6;
    a7 = 7;
    a8 = 8;
    a9 = 9;
    a10 = 0;
    a11 = 1;
    a12 = 2;
    a13 = 3;
    a14 = 4;
    a15 = 5;
//     a16 = 0;
//     a17 = 7;
//     a18 = 8;
//     a19 = 9;
//     a20 = 0;
//     a21 = 1;
//     a22 = 2;
//     a23 = 3;
//     a24 = 4;
//     a25 = 5;
//     a26 = 6;
//     a27 = 7;
//     a28 = 8;
//     a29 = 9;
//     a30 = 0;
//     a31 = 1;
    a0 = testParam8(a0, a1, a2, a3, a4, a5, a6, a7);
    putint(a0);putch(32);
    a0 = testParam16(a0, a1, a2, a3,
            a4, a5, a6, a7,
            a8, a9, a10, a11,
            a12, a13, a14, a15);
    putint(a0);putch(32);
//     a0 = testParam32(a0, a1, a2, a3,
//             a4, a5, a6, a7,
//             a8, a9, a10, a11,
//             a12, a13, a14, a15,
//             a16, a17, a18, a19,
//             a20, a21, a22, a23,
//             a24, a25, a26, a27,
//             a28, a29, a30, a31);
//     putint(a0);putch(32);
    return 0;
}