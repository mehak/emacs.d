(defun convert-to-nether (x z)
  "Converts overworld x and z to nether coordinates"
  (let ((nx (/ x 8.0))
        (nz (/ z 8.0)))
    (list nx nz)))
