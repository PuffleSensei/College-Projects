import matplotlib.pyplot as plt

# Data from the provided text
data = """
KLBfa 12.0788 12.5179 12.1533 11.8308 11.9268 11.7865
KLDic 11.1009 11.7767 11.9021 10.0155 10.3028 10.7928
KLProb 10.2467 10.1164 10.4478 9.3486 9.4993 9.5537
KLAll 5.6193 6.1912 6.9749 3.2784 5.1186 5.3429
KLBfa 7.8006 2.0598 1.1105 8.5515 10.8753 6.4370
KLDic 0.3519 0.2727 0.1161 0.5221 1.0597 0.2488
KLProb 1.6762 1.8614 1.1436 5.4648 2.0630 2.0235
KLAll -8.8994 -7.2047 -5.3692 -7.5933 -4.6160 -3.9984
KLBfa -3.0046 -1.4362 -2.3564 -2.2791 -2.3951 -1.2541
KLDic 5.5556 5.9258 1.1966 3.4309 3.0581 1.8182
KLDic 13.0063 16.6061 13.8439 11.1163 11.6300 11.0145
KLProb 12.0506 12.0341 10.7793 6.9454 6.9880 6.7100
KLAll 9.2533 9.4217 5.8988 5.8993 5.3360 3.3893
KLBfa 0.6410 0.9677 1.1538 1.0628 0.9133 0.6623
KLDic 1.8715 2.9528 1.5388 3.4172 5.6578 2.0859
KLProb 0.9765 1.6174 0.9335 2.6606 3.0942 1.7423
KLAll 1.1992 1.5318 2.8269 4.1227 3.2999 6.3708
KLBfa 8.6483 6.7929 3.6468 4.0480 1.7946 1.3838
KLDic 0.6029 0.6291 0.3974 0.3833 0.5417 0.3120
KLProb 0.3492 0.4390 0.3814 1.8581 1.1519 1.2526
KLAll 6.7505 1.8783 0.9242 6.7797 6.5693 4.6571
KLBfa 1.2415 1.7885 1.3490 0.3299 0.7236 0.5631
KLDic 4.1313 6.3760 4.1089 2.2732 2.6128 2.6266
KLProb 0.3446 0.6661 0.5040 1.1542 2.5521 1.7725
KLAll 3.0218 3.3403 2.8551 3.5520 2.1229 2.4907
KLBfa 2.1916 3.6398 2.9347 1.1732 1.7442 1.1001
KLDic 2.6813 5.8958 2.7036 1.0440 2.1223 2.0496
KLProb 0.7428 1.1549 0.7512 0.9996 1.7949 1.2733
KLAll 2.8681 3.8673 4.4639 1.6443 2.0144 2.3132
KLBfa 2.8831 4.2145 2.7957 1.4231 1.5154 1.2219
KLDic 4.3938 7.6722 4.0757 2.0382 3.7266 2.8444
KLProb 0.9770 1.5533 1.1632 1.3931 1.8223 1.4169
KLAll 4.3930 4.6782 5.4262 4.7708 2.6660 2.6944
KLBfa 2.5697 2.2666 3.9500 2.8319 2.2526 2.8474
KLDic 1.0812 2.0236 0.8438 0.1939 0.5237 0.3673
KLProb 0.2886 0.4965 0.9081 1.0141 0.9805 0.9177
KLAll 2.2322 2.6261 3.9691 1.6662 0.8765 1.3227
KLBfa 1.6920 1.6607 0.3271 0.6190 1.6402 0.3176
KLDic 0.2456 0.3219 0.1479 0.1504 0.1692 0.2607
KLProb 0.0058 0.0127 0.0080 0.1873 0.4803 0.0319
KLAll 1.4990 1.3592 0.2232 0.3698 0.8558 0.2026
KLBfa 3.7802 0.2433 0.3576 3.6622 5.6669 3.2280
KLDic 0.6652 0.0574 0.1811 0.4127 0.4552 0.0112
KLProb 0.0545 0.0364 0.0688 0.9957 0.8549 0.1223
KLAll 3.3667 0.2370 0.2617 2.7974 3.0522 2.6380
"""

# Parse data into lists
labels = []
values = []
for line in data.strip().split('\n'):
    parts = line.split()
    if parts[0] == 'KLBfa':
        parts[0] = 'fuzzyPSM [17]'
    elif parts[0] == 'KLDic':
        parts[0] = 'MultiPSM [19]'
    elif parts[0] == 'KLProb':
        parts[0] = 'PCFG-PSM [25]'
    elif parts[0] == 'KLAll':
        parts[0] = 'Markov-PSM [16]'
    elif parts[0] == 'KLBfa':
        parts[0] = 'RNN-PSM [18]'
    elif parts[0] == 'KLDic':
        parts[0] = 'LPSE [13]'
    labels.append(parts[0])
    values.append([float(x) for x in parts[1:]])

# Transpose the values for plotting
values_transposed = list(map(list, zip(*values)))

# Plotting
plt.figure(figsize=(10, 6))
for label, data in zip(labels, values_transposed):
    plt.plot(data, label=label)

# Add a solid horizontal line at y=2 with thicker and grey color
plt.axhline(y=2, color='grey', linestyle='-', linewidth=2)

plt.xlabel('Top k passwords in the test set')
plt.ylabel('Values')
plt.title('PSM Metrics')
plt.legend()
plt.grid(True)
plt.xticks(range(len(values[0])))

# Set the background color to black
plt.gca().set_facecolor('black')

plt.show()
