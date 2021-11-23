# Picture Type

```purescript
pure $ textBaseLine BaselineHanging $ text Fill "Hello world"
```

Here, text is drawn and `textBaseLine` is set to the top of the text. If you don't do this, the text will stick out of the screen.

You can change the font of the text.

```purescript
pure $ font testFont $ textBaseLine BaselineHanging $ text Fill "Hello world"

...

testFont :: Font
testFont = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 40
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }
```
(I plan to simplify this setting in the future.)


You can also move it and rotate it.

```purescript
pure $ translate 50.0 50.0 $ rotate (0.5 * pi) $ font testFont $ textBaseLine BaselineHanging $ text Fill "Hello world"
```
>Note: The center of rotation is always (0, 0).

It will be easier to understand if you write it like a procedure using `#`.

```purescript
pure $ text Fill "Hello world"
  # textBaseLine BaselineHanging
  # font testFont
  # rotate (0.5 * pi)
  # translate 50.0 50.0
```

- Draws a rectangle with the `rect` function.

```purescript
pure $ rect Fill 40.0 50.0
  # translate 50.0 50.0
```

- Draw an arc with the `arc` function.

```purescript
pure $ arc { start: pi / 4.0, angle: pi / 2.0, radius: 30.0 }
  # translate 50.0 50.0
```
>Note: Draws clockwise from the angle specified by `start` by the angle specified by `angle`.
If `angle` is negative, it will be drawn counter-clockwise.
If `angle` is not in the range -2π to 2π, then 4π is added or subtracted to bring it into the range.

- Draw a polygon with the `polygon` function.

```purescript
pure $ polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
```

- Change `Fill` to `Stroke` to draw a border.

```purescript
pure $ polygon Stroke [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
pure $ rect Stroke 40.0 50.0
```

- Change the lineWidth of a line with `lineWidth`.

```purescript
pure $ polygon Stroke [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # lineWidth 10.0
```

>Note: For functions that cannot be combined, such as `lineWidth`, the innermost one will be applied.


```purescript
pure $ polygon Stroke [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # lineWidth 10.0
  # lineWidth 20.0 --invalid
```

- You can change the color of the image with `color`.

```purescript
pure $ polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # color (rgb 225 0 225)
```

- You can use `<-^` to pile up a `Picture`.

```purescript
pure $ poly1 <-^ poly2

...

poly1 = polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # color (rgb 225 0 225)

poly2 = polygon Fill [ 100.0 /\ 100.0, 200.0 /\ 100.0, 20.0 /\ 30.0 ]
  # color (rgb 0 0 225)
```

- You can use `image` to draw an image.

```purescript
pure $ image "/images/hoge.png"
```

There are many more functions, but their documents are under preparation.