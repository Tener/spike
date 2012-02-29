#include <libsoup/soup.h>
#include <libsoup/soup-gnome.h>
#include <webkit/webkit.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libsoup/soup.h>
#include <webkit/webkit.h>

void spike_setup_webkit_globals(char * web_database_path, char * cookie_jar_path)
{
  //eg. /tmp/spike.webkit.db
  webkit_set_web_database_directory_path(web_database_path);

  
  SoupSession * ses = webkit_get_default_session();
  //eg. /tmp/spike.webkit.cookie.db
  SoupCookieJar * jar = soup_cookie_jar_sqlite_new(cookie_jar_path, 0);
  soup_session_add_feature( ses, SOUP_SESSION_FEATURE (jar));
}
