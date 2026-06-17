# Make a sound and send an email when a process finished

The notify_finished make a sound using beepr::beep, compose and email
and send it returing the blastula::smtp_send call results.

## Usage

``` r
notify_finished(name, body = "", ..., sound = 1, tictoc_result = NULL)
```

## Arguments

- name:

  The process name (Required)

- body:

  The contents of the email (Default "")

- ...:

  Additional arguments to pass to the template function. If you're using
  the default template, you can use font_family to control the base
  font, and content_width to control the width of the main content; see
  blastula_template(). By default, the content_width is set to 1000px.
  Using widths less than 600px is generally not advised but, if
  necessary, be sure to test such HTML emails with a wide range of email
  clients before sending to the intended recipients. The Outlook mail
  client (Windows, Desktop) does not respect content_width.

- sound:

  The sound for beepr::beep call (Default 1)

- tictoc_result:

  the result from tictoc::toc (Default NULL)

## Details

The following environment variables should be set:

- MY_SMTP_USER from

- MY_SMTP_RECIPIENT to

- MY_SMTP_PASSWORD service password (for gmail you can use
  https://myaccount.google.com/apppasswords)

- MY_SMTP_PROVIDER blastula provider (gmail if not set)

## Examples

``` r
if (exists("not_run")) {
  tictoc::tic()
  Sys.sleep(1)
  jrrosell::notify_finished("job", "Well done", sound = "fanfare", tictoc_result = tictoc::toc())
}
```
