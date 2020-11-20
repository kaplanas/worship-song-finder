USE wsf_public;

DROP TABLE IF EXISTS wsf_public.accidentals;
CREATE TABLE wsf_public.accidentals AS
(SELECT accidentals.*
 FROM wsf.accidentals);
COMMIT;

DROP TABLE IF EXISTS wsf_public.all_lyrics;
CREATE TABLE wsf_public.all_lyrics AS
(SELECT all_lyrics.*
 FROM wsf.all_lyrics
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_first_lines) include_lyrics
      ON all_lyrics.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.alternativetunes;
CREATE TABLE wsf_public.alternativetunes AS
(SELECT alternativetunes.*
 FROM wsf.alternativetunes
      JOIN (SELECT DISTINCT AlternativeTuneID
            FROM wsf_shiny.psalmsongs_alternativetunes) include_alternativetunes
      ON alternativetunes.AlternativeTuneID = include_alternativetunes.AlternativeTuneID);
CREATE INDEX alternativetunes_idx ON wsf_public.alternativetunes (TuneID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.arrangements;
CREATE TABLE wsf_public.arrangements AS
(SELECT arrangements.*
 FROM wsf.arrangements
      JOIN (SELECT DISTINCT ArrangementID
            FROM wsf_shiny.songinstances) include_arrangement
      ON arrangements.ArrangementID = include_arrangement.ArrangementID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.arrangements_arrangementtypes;
CREATE TABLE wsf_public.arrangements_arrangementtypes AS
(SELECT arrangements_arrangementtypes.*
 FROM wsf.arrangements_arrangementtypes
      JOIN (SELECT DISTINCT ArrangementID
            FROM wsf_shiny.songinstances) include_arrangement
      ON arrangements_arrangementtypes.ArrangementID = include_arrangement.ArrangementID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.arrangements_artists;
CREATE TABLE wsf_public.arrangements_artists AS
(SELECT arrangements_artists.*
 FROM wsf.arrangements_artists
      JOIN (SELECT DISTINCT ArrangementID
            FROM wsf_shiny.songinstances) include_arrangement
      ON arrangements_artists.ArrangementID = include_arrangement.ArrangementID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.arrangements_copyrightholders;
CREATE TABLE wsf_public.arrangements_copyrightholders AS
(SELECT arrangements_copyrightholders.*
 FROM wsf.arrangements_copyrightholders
      JOIN (SELECT DISTINCT ArrangementID
            FROM wsf_shiny.songinstances) include_arrangement
      ON arrangements_copyrightholders.ArrangementID = include_arrangement.ArrangementID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.arrangements_tunes;
CREATE TABLE wsf_public.arrangements_tunes AS
(SELECT arrangements_tunes.*
 FROM wsf.arrangements_tunes
      JOIN (SELECT DISTINCT ArrangementID
            FROM wsf_shiny.songinstances) include_arrangement
      ON arrangements_tunes.ArrangementID = include_arrangement.ArrangementID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.arrangementtypes;
CREATE TABLE wsf_public.arrangementtypes AS
(SELECT arrangementtypes.*
 FROM wsf.arrangementtypes);
COMMIT;

DROP TABLE IF EXISTS wsf_public.artists;
CREATE TABLE wsf_public.artists AS
(SELECT artists.*
 FROM wsf.artists
      JOIN (SELECT DISTINCT ArtistID
            FROM wsf_shiny.artists) include_artist
      ON artists.ArtistID = include_artist.ArtistID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.booksofthebible;
CREATE TABLE wsf_public.booksofthebible AS
(SELECT booksofthebible.*
 FROM wsf.booksofthebible);
COMMIT;

DROP TABLE IF EXISTS wsf_public.copyrightadministrators;
CREATE TABLE wsf_public.copyrightadministrators AS
(SELECT copyrightadministrators.*
 FROM wsf.copyrightadministrators);
COMMIT;

DROP TABLE IF EXISTS wsf_public.copyrightholders;
CREATE TABLE wsf_public.copyrightholders AS
(SELECT copyrightholders.*
 FROM wsf.copyrightholders
      LEFT JOIN (SELECT DISTINCT CopyrightHolderID
                 FROM wsf.lyrics_copyrightholders
                      JOIN wsf_shiny.lyrics_artists
                      ON lyrics_copyrightholders.LyricsID = lyrics_artists.LyricsID) include_lyrics
      ON copyrightholders.CopyrightHolderID = include_lyrics.CopyrightHolderID
      LEFT JOIN (SELECT DISTINCT CopyrightHolderID
                 FROM wsf.tunes_copyrightholders
                      JOIN wsf.songinstances_tunes
                      ON tunes_copyrightholders.TuneID = songinstances_tunes.TuneID
                      JOIN wsf_shiny.songinstances
                      ON songinstances_tunes.SongInstanceID = songinstances.SongInstanceID) include_tune
      ON copyrightholders.CopyrightHolderID = include_tune.CopyrightHolderID
      LEFT JOIN (SELECT DISTINCT CopyrightHolderID
                 FROM wsf.arrangements_copyrightholders
                      JOIN wsf_shiny.songinstances
                      ON arrangements_copyrightholders.ArrangementID = songinstances.ArrangementID) include_arrangement
      ON copyrightholders.CopyrightHolderID = include_arrangement.CopyrightHolderID
 WHERE include_lyrics.CopyrightHolderID IS NOT NULL
       OR include_tune.CopyrightHolderID IS NOT NULL
       OR include_arrangement.CopyrightHolderID IS NOT NULL);
COMMIT;

DROP TABLE IF EXISTS wsf_public.genders;
CREATE TABLE wsf_public.genders AS
(SELECT genders.*
 FROM wsf.genders);
COMMIT;

DROP TABLE IF EXISTS wsf_public.keysignatures;
CREATE TABLE wsf_public.keysignatures AS
(SELECT keysignatures.*
 FROM wsf.keysignatures
      JOIN (SELECT KeySignatureID
            FROM wsf_shiny.keysignatures) include_keysignature
      ON keysignatures.KeySignatureID = include_keysignature.KeySignatureID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.languages;
CREATE TABLE wsf_public.languages AS
(SELECT languages.*
 FROM wsf.languages
      JOIN (SELECT DISTINCT LanguageID
            FROM wsf_shiny.languages) include_language
      ON languages.LanguageID = include_language.LanguageID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics;
CREATE TABLE wsf_public.lyrics AS
(SELECT lyrics.*
 FROM wsf.lyrics
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics_artists;
CREATE TABLE wsf_public.lyrics_artists AS
(SELECT lyrics_artists.*
 FROM wsf.lyrics_artists
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics_artists.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics_copyrightholders;
CREATE TABLE wsf_public.lyrics_copyrightholders AS
(SELECT lyrics_copyrightholders.*
 FROM wsf.lyrics_copyrightholders
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics_copyrightholders.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics_files;
CREATE TABLE wsf_public.lyrics_files AS
(SELECT lyrics_files.*
 FROM wsf.lyrics_files
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics_files.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics_meters;
CREATE TABLE wsf_public.lyrics_meters AS
(SELECT lyrics_meters.*
 FROM wsf.lyrics_meters
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics_meters.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics_scripturereferences;
CREATE TABLE wsf_public.lyrics_scripturereferences AS
(SELECT lyrics_scripturereferences.*
 FROM wsf.lyrics_scripturereferences
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics_scripturereferences.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.lyrics_translations;
CREATE TABLE wsf_public.lyrics_translations AS
(SELECT lyrics_translations.*
 FROM wsf.lyrics_translations
      JOIN (SELECT DISTINCT LyricsID
            FROM wsf_shiny.lyrics_artists) include_lyrics
      ON lyrics_translations.LyricsID = include_lyrics.LyricsID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.meters;
CREATE TABLE wsf_public.meters AS
(SELECT meters.*
 FROM wsf.meters
      JOIN (SELECT DISTINCT MeterID
            FROM wsf_shiny.meters) include_meter
      ON meters.MeterID = include_meter.MeterID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.metricalpsalms;
CREATE TABLE wsf_public.metricalpsalms AS
(SELECT metricalpsalms.*
 FROM wsf.metricalpsalms);
COMMIT;

DROP TABLE IF EXISTS wsf_public.metricalpsalms_lyrics;
CREATE TABLE wsf_public.metricalpsalms_lyrics AS
(SELECT metricalpsalms_lyrics.*
 FROM wsf.metricalpsalms_lyrics);
COMMIT;

DROP TABLE IF EXISTS wsf_public.metricalpsalms_prettyscripturelists;
CREATE TABLE wsf_public.metricalpsalms_prettyscripturelists AS
(SELECT metricalpsalms_prettyscripturelists.*
 FROM wsf.metricalpsalms_prettyscripturelists);
COMMIT;

DROP TABLE IF EXISTS wsf_public.modes;
CREATE TABLE wsf_public.modes AS
(SELECT modes.*
 FROM wsf.modes);
COMMIT;

DROP TABLE IF EXISTS wsf_public.pitches;
CREATE TABLE wsf_public.pitches AS
(SELECT pitches.*
 FROM wsf.pitches);
COMMIT;

DROP TABLE IF EXISTS wsf_public.prettyscripturelists;
CREATE TABLE wsf_public.prettyscripturelists AS
(SELECT prettyscripturelists.*
 FROM wsf.prettyscripturelists
      JOIN (SELECT DISTINCT SongInstanceID
            FROM wsf_shiny.songinstances) include_songinstance
      ON prettyscripturelists.SongInstanceID = include_songinstance.SongInstanceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.psalmsongs;
CREATE TABLE wsf_public.psalmsongs AS
(SELECT psalmsongs.*
 FROM wsf.psalmsongs
      JOIN wsf_shiny.songs
      ON psalmsongs.SongID = songs.SongID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.psalmsongs_prettyscripturelists;
CREATE TABLE wsf_public.psalmsongs_prettyscripturelists AS
(SELECT psalmsongs_prettyscripturelists.*
 FROM wsf.psalmsongs_prettyscripturelists
      JOIN wsf_public.psalmsongs
      ON psalmsongs_prettyscripturelists.PsalmSongID = psalmsongs.PsalmSongID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.psalmsongtypes;
CREATE TABLE wsf_public.psalmsongtypes AS
(SELECT psalmsongtypes.*
 FROM wsf.psalmsongtypes);
COMMIT;

DROP TABLE IF EXISTS wsf_public.scripturereferences;
CREATE TABLE wsf_public.scripturereferences AS
(SELECT scripturereferences.*
 FROM wsf.scripturereferences
      JOIN (SELECT DISTINCT ScriptureReferenceID
            FROM wsf_shiny.scripturereferences) include_scripturereference
      ON scripturereferences.ScriptureReferenceID = include_scripturereference.ScriptureReferenceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songbookentries;
CREATE TABLE wsf_public.songbookentries AS
(SELECT songbookentries.*
 FROM wsf.songbookentries
      JOIN wsf_shiny.songbooks
      ON songbookentries.SongbookID = songbooks.SongbookID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songbooks;
CREATE TABLE wsf_public.songbooks AS
(SELECT songbooks.*
 FROM wsf.songbooks
      JOIN wsf_shiny.songbooks include_songbook
      ON songbooks.SongbookID = include_songbook.SongbookID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songbookvolumes;
CREATE TABLE wsf_public.songbookvolumes AS
(SELECT songbookvolumes.*
 FROM wsf.songbookvolumes
      JOIN (SELECT DISTINCT SongbookVolumeID
            FROM wsf_shiny.songinstances_songbooks) include_volume
      ON songbookvolumes.SongbookVolumeID = include_volume.SongbookVolumeID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songinstances;
CREATE TABLE wsf_public.songinstances AS
(SELECT songinstances.*
 FROM wsf.songinstances
      JOIN wsf_shiny.songinstances include_songinstance
      ON songinstances.SongInstanceID = include_songinstance.SongInstanceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songinstances_keysignatures;
CREATE TABLE wsf_public.songinstances_keysignatures AS
(SELECT songinstances_keysignatures.*
 FROM wsf.songinstances_keysignatures
      JOIN wsf_shiny.songinstances
      ON songinstances_keysignatures.SongInstanceID = songinstances.SongInstanceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songinstances_lyrics;
CREATE TABLE wsf_public.songinstances_lyrics AS
(SELECT songinstances_lyrics.*
 FROM wsf.songinstances_lyrics
      JOIN wsf_shiny.songinstances
      ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songinstances_timesignatures;
CREATE TABLE wsf_public.songinstances_timesignatures AS
(SELECT songinstances_timesignatures.*
 FROM wsf.songinstances_timesignatures
      JOIN wsf_shiny.songinstances
      ON songinstances_timesignatures.SongInstanceID = songinstances.SongInstanceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songinstances_tunes;
CREATE TABLE wsf_public.songinstances_tunes AS
(SELECT songinstances_tunes.*
 FROM wsf.songinstances_tunes
      JOIN wsf_shiny.songinstances
      ON songinstances_tunes.SongInstanceID = songinstances.SongInstanceID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songs;
CREATE TABLE wsf_public.songs AS
(SELECT songs.*
 FROM wsf.songs
      JOIN wsf_shiny.songs include_song
      ON songs.SongID = include_song.SongID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.songs_topics;
CREATE TABLE wsf_public.songs_topics AS
(SELECT songs_topics.*
 FROM wsf.songs_topics
      JOIN wsf_shiny.songs
      ON songs_topics.SongID = songs.SongID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.timesignatures;
CREATE TABLE wsf_public.timesignatures AS
(SELECT timesignatures.*
 FROM wsf.timesignatures
      JOIN wsf_shiny.timesignatures include_timesignature
      ON timesignatures.TimeSignatureID = include_timesignature.TimeSignatureID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.topics;
CREATE TABLE wsf_public.topics AS
(SELECT topics.*
 FROM wsf.topics);
COMMIT;

DROP TABLE IF EXISTS wsf_public.tunes;
CREATE TABLE wsf_public.tunes AS
(SELECT tunes.*
 FROM wsf.tunes
      LEFT JOIN (SELECT DISTINCT TuneID
                 FROM wsf.songinstances_tunes) any_song
      ON tunes.TuneID = any_song.TuneID
      LEFT JOIN (SELECT DISTINCT TuneID
                 FROM wsf_shiny.songinstances
                      JOIN wsf.songinstances_tunes
                      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID) include_tune
      ON tunes.TuneID = include_tune.TuneID
 WHERE include_tune.TuneID IS NOT NULL
       OR any_song.TuneID IS NULL);
CREATE INDEX tunes_idx ON wsf_public.tunes (TuneID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.tunes_artists;
CREATE TABLE wsf_public.tunes_artists AS
(SELECT tunes_artists.*
 FROM wsf.tunes_artists
      JOIN wsf_public.tunes
      ON tunes_artists.TuneID = tunes.TuneID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.tunes_canonicalsongs;
CREATE TABLE wsf_public.tunes_canonicalsongs AS
(SELECT tunes_canonicalsongs.*
 FROM wsf.tunes_canonicalsongs
      JOIN wsf_public.tunes
      ON tunes_canonicalsongs.TuneID = tunes.TuneID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.tunes_copyrightholders;
CREATE TABLE wsf_public.tunes_copyrightholders AS
(SELECT tunes_copyrightholders.*
 FROM wsf.tunes_copyrightholders
      JOIN wsf_public.tunes
      ON tunes_copyrightholders.TuneID = tunes.TuneID);
COMMIT;

DROP TABLE IF EXISTS wsf_public.tunes_meters;
CREATE TABLE wsf_public.tunes_meters AS
(SELECT tunes_meters.*
 FROM wsf.tunes_meters
      JOIN wsf_public.tunes
      ON tunes_meters.TuneID = tunes.TuneID);
COMMIT;