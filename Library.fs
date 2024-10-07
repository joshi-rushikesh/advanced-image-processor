//
// Author: Rushikesh Joshi
// Title – Advanced Image Processor
// Description: This module contains a collection of functions to perform various
//              image processing operations on PPM images. It includes functions
//              for applying a sepia tone, increasing the intensity of a color channel,
//              flipping an image horizontally, rotating an image 180 degrees, and
//              performing edge detection to highlight the outlines within an image.
//
// 
//

namespace ImageLibrary

module Operations =
  //
  // Sepia:
  //
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //
  let rec Sepia (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // 'clamp' function to constrain color values to the maximum value of 255
    let clamp value = min value 255

    // 'sepiaTone' Function to convert a single pixel to its sepia tone equivalent
    let sepiaTone (r, g, b) = 
        // Calculate the new red, green, and blue values using the sepia formula
        let newRed = clamp (int (0.393 * float r + 0.769 * float g + 0.189 * float b))
        let newGreen = clamp (int (0.349 * float r + 0.686 * float g + 0.168 * float b))
        let newBlue = clamp (int (0.272 * float r + 0.534 * float g + 0.131 * float b))
        // Return the new sepia tone pixel
        (newRed, newGreen, newBlue)

    // Apply the sepia tone conversion to each pixel in the image and return updated image
    image |> List.map (fun row -> row |> List.map sepiaTone)


  //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //
  let rec IncreaseIntensity (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (intensity:double)
                    (channel:char) = 
    // 'clamp' function constrains a value to be within the range of 0 to 255
    let clamp value = min (max value 0) 255

    // 'adjustColor' function applies the intensity scaling to a color value
    let adjustColor value = clamp (int (float value * intensity))

    // 'adjustPixel' function applies the intensity adjustment to the selected channel of a pixel
    let adjustPixel (r, g, b) =
        match channel with
        | 'r' -> (adjustColor r, g, b) // Intensify the red channel if selected
        | 'g' -> (r, adjustColor g, b) // Intensify the green channel if selected
        | 'b' -> (r, g, adjustColor b) // Intensify the blue channel if selected
        | _ -> (r, g, b) // If the channel is not recognized, return the pixel unchanged

    // 'adjustRow' function applies the intensity adjustment to each pixel in a row
    let adjustRow row = row |> List.map adjustPixel

    // 'adjustedImage' is the result of applying 'adjustRow' to each row of the image
    let adjustedImage = image |> List.map adjustRow
    
    // The function returns the new image with the intensity adjustment applied
    adjustedImage


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // 'flipRow' function takes a single row list of pixels and returns its reverse
    let flipRow row = 
        List.rev row // Reverse the row to flip it horizontally

    // 'flippedImage' applies the 'flipRow' function to each row of the original image
    // It utilizes the 'List.map' function to transform each row in the image list
    let flippedImage = 
        image |> List.map flipRow // Map the flipRow function over each row in the image

    // Return the new image with rows flipped
    flippedImage


  //
  // Rotate180:
  //
  // Rotates the image 180 degrees.
  //
  // Returns: updated image.
  //
  let rec Rotate180 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // 'flipHorizontal' function applies a horizontal flip to the image by reversing each row
    let flipHorizontal img = 
        img |> List.map List.rev

    // 'flipVertical' function applies a vertical flip to the image by reversing the order of rows
    let flipVertical img = 
        List.rev img

    // The image is first flipped horizontally and then vertically to achieve a 180-degree rotation
    image
    |> flipHorizontal
    |> flipVertical
    // The resulting image is then returned


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) = 
  
    // 'colorDistance' calculates the color distance between two pixels based on their RGB values
    let colorDistance (r1, g1, b1) (r2, g2, b2) =
        let dr, dg, db = float (r1 - r2), float (g1 - g2), float (b1 - b2)
        sqrt (dr * dr + dg * dg + db * db)

    // 'isEdge' checks if a pixel at position (x, y) in the row is an edge pixel by comparing
    // it to its right and below neighbors using the color distance and threshold.
    let isEdge (x, y) row =
        let current = List.item x row
        let rightNeighbor = if x < width - 1 then List.item (x + 1) row else current
        let belowNeighbor = if y < height - 1 then List.item x (List.item (y + 1) image) else current
        colorDistance current rightNeighbor > float threshold || colorDistance current belowNeighbor > float threshold

    // 'processRow' applies the edge detection to a single row, marking edge pixels black(0,0,0) and non-edge pixels white(255, 255, 255)
    let processRow y row =
        row
        |> List.mapi (fun x pixel -> if isEdge (x, y) row then (0, 0, 0) else (255, 255, 255))
        |> List.take (width - 1) // Exclude the last pixel which has no right neighbor

    // 'processedImage' constructs the new image after applying edge detection to each row so it is applied 
    // to the entire image, excluding the last row which has no below neighbor.
    let processedImage = 
        image 
        |> List.mapi processRow
        |> List.take (height - 1) // Exclude the last row

    // Return the processed image with edge detection applied
    processedImage