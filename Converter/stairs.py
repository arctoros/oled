from PIL import Image, ImageOps

file = open('image.bin', 'wb')
counter = 1
result = 0

for x in range(128):
    if x % 2 == 0:
        result = pow(2, counter) - 1
        counter += 1
    hexdec = result.to_bytes(8, byteorder='little')
    file.write(hexdec)
