USE wsf_shiny;

-- Table of songs
DROP TABLE IF EXISTS wsf_shiny.songs;
CREATE TABLE wsf_shiny.songs AS
(SELECT SongID,
        SongName
 FROM wsf.songs);
COMMIT;

-- Table of song instances
DROP TABLE IF EXISTS wsf_shiny.songinstances;
CREATE TABLE wsf_shiny.songinstances AS
(SELECT songinstances.SongInstanceID,
        SongInstance, songinstances.ArrangementID,
        SongID,
        LastLyricsYear,
        LyricsCopyright,
        LastTuneYear,
        TuneCopyright,
        ArrangementCopyright
 FROM wsf.songinstances 
      LEFT JOIN (SELECT SongInstanceID,
                        MAX(CopyrightYear) AS LastLyricsYear,
                        GROUP_CONCAT(CONCAT('(C) ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS LyricsCopyright
                 FROM wsf.songinstances_lyrics
                      JOIN (SELECT lyrics.LyricsID,
                                   CopyrightYear,
                                   GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                          THEN CopyrightHolderName
                                                END
                                                ORDER BY CopyrightHolderName
                                                SEPARATOR ', ') AS CopyrightHolderNames
                            FROM wsf.lyrics
                                 JOIN wsf.lyrics_copyrightholders
                                      ON lyrics.LyricsID = lyrics_copyrightholders.LyricsID
                                 JOIN wsf.copyrightholders
                                      ON lyrics_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                            GROUP BY lyrics.LyricsID,
                                     CopyrightYear) lyrics
                           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
                 GROUP BY SongInstanceID) lyrics
      ON songinstances.SongInstanceID = lyrics.SongInstanceID
      LEFT JOIN (SELECT SongInstanceID,
                        MAX(CopyrightYear) AS LastTuneYear,
                        GROUP_CONCAT(CONCAT('(C) ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS TuneCopyright
                 FROM wsf.songinstances_tunes
                      JOIN (SELECT tunes.TuneID,
                                   CopyrightYear,
                                   GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                          THEN CopyrightHolderName
                                                END
                                                ORDER BY CopyrightHolderName
                                                SEPARATOR ', ') AS CopyrightHolderNames
                            FROM wsf.tunes
                                 JOIN wsf.tunes_copyrightholders
                                      ON tunes.TuneID = tunes_copyrightholders.TuneID
                                 JOIN wsf.copyrightholders
                                      ON tunes_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                            GROUP BY tunes.TuneID,
                                     CopyrightYear) tunes
                           ON songinstances_tunes.TuneID = tunes.TuneID
                 GROUP BY SongInstanceID) tunes
      ON songinstances.SongInstanceID = tunes.SongInstanceID
      LEFT JOIN (SELECT ArrangementID,
                        GROUP_CONCAT(CONCAT('(C) ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS ArrangementCopyright
                 FROM (SELECT arrangements.ArrangementID,
                              CopyrightYear,
                              GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                     THEN CopyrightHolderName
                                           END
                                           ORDER BY CopyrightHolderName
                                           SEPARATOR ', ') AS CopyrightHolderNames
                       FROM wsf.arrangements
                            JOIN wsf.arrangements_copyrightholders
                                 ON arrangements.ArrangementID = arrangements_copyrightholders.ArrangementID
                            JOIN wsf.copyrightholders
                                 ON arrangements_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                       GROUP BY arrangements.ArrangementID,
                                CopyrightYear) a
                 GROUP BY ArrangementID) arrangements
      ON songinstances.ArrangementID = arrangements.ArrangementID);
COMMIT;

-- Table of artists
DROP TABLE IF EXISTS wsf_shiny.artists;
CREATE TABLE wsf_shiny.artists AS
(SELECT ArtistID,
        LastName,
        FirstName,
        GenderName
 FROM wsf.artists
      LEFT JOIN wsf.genders
      ON artists.GenderID = genders.GenderID);
COMMIT;

-- Table that connects song instances and artists
DROP TABLE IF EXISTS wsf_shiny.songinstances_artists;
CREATE TABLE wsf_shiny.songinstances_artists AS
(SELECT DISTINCT songinstances.SongInstanceID,
        SongID, ArtistID,
        'lyricist' AS Role
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_artists
      ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
 UNION ALL
 SELECT DISTINCT songinstances.SongInstanceID,
        SongID, ArtistID,
        'composer' AS Role
 FROM wsf.songinstances
      JOIN wsf.songinstances_tunes
      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
      JOIN wsf.tunes_artists
      ON songinstances_tunes.TuneID = tunes_artists.TuneID
 UNION ALL
 SELECT DISTINCT songinstances.SongInstanceID,
        SongID, ArtistID, 'arranger' AS Role
 FROM wsf.songinstances
      JOIN wsf.arrangements_artists
      ON songinstances.ArrangementID = arrangements_artists.ArrangementID);
COMMIT;

-- Table of topics
DROP TABLE IF EXISTS wsf_shiny.topics;
CREATE TABLE wsf_shiny.topics AS
(SELECT TopicID,
        TopicName
 FROM wsf.topics);
COMMIT;

-- Table that connects songs and topics
DROP TABLE IF EXISTS wsf_shiny.songs_topics;
CREATE TABLE wsf_shiny.songs_topics AS
(SELECT SongID, TopicID
 FROM wsf.songs_topics);
COMMIT;

-- Table of books of the Bible
DROP TABLE IF EXISTS wsf_shiny.bible_books;
CREATE TABLE wsf_shiny.bible_books AS
(SELECT BookID,
        BookName,
        BookAbbreviation
 FROM wsf.booksofthebible);
COMMIT;

-- Table of scripture references
DROP TABLE IF EXISTS wsf_shiny.scripturereferences;
CREATE TABLE wsf_shiny.scripturereferences AS
(SELECT ScriptureReferenceID,
        booksofthebible.BookID,
        BookName,
        BookAbbreviation,
        Chapter,
        Verse
 FROM wsf.scripturereferences
      JOIN wsf.booksofthebible
      ON scripturereferences.BookID = booksofthebible.BookID);
COMMIT;

-- Table that connects song instances and scripture references
DROP TABLE IF EXISTS wsf_shiny.songinstances_scripturereferences;
CREATE TABLE wsf_shiny.songinstances_scripturereferences AS
(SELECT DISTINCT songinstances.SongInstanceID, SongID, ScriptureReferenceID
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_scripturereferences
      ON songinstances_lyrics.LyricsID = lyrics_scripturereferences.LyricsID);
COMMIT;

-- Table of languages
DROP TABLE IF EXISTS wsf_shiny.languages;
CREATE TABLE wsf_shiny.languages AS
(SELECT LanguageID,
        LanguageName
 FROM wsf.languages);
COMMIT;

-- Table that connects song instances and languages
DROP TABLE IF EXISTS wsf_shiny.songinstances_languages;
CREATE TABLE wsf_shiny.songinstances_languages AS
(SELECT DISTINCT songinstances.SongInstanceID,
        SongID, LanguageID
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID);
COMMIT;

-- Table of songbooks
DROP TABLE IF EXISTS wsf_shiny.songbooks;
CREATE TABLE wsf_shiny.songbooks AS
(SELECT SongbookID,
        SongbookName,
        SongbookAbbreviation,
        IncludeInSearch
 FROM wsf.songbooks);
COMMIT;

-- Table that conects song instances and songbooks
DROP TABLE IF EXISTS wsf_shiny.songinstances_songbooks;
CREATE TABLE wsf_shiny.songinstances_songbooks AS
(SELECT songinstances.SongInstanceID,
        SongID,
        songbooks.SongbookID,
        SongbookName,
        SongbookAbbreviation,
        IncludeInSearch,
        songbookvolumes.SongbookVolumeID,
        SongbookVolume,
        EntryNumber
 FROM wsf.songinstances
      JOIN wsf.songbookentries
      ON songinstances.SongInstanceID = songbookentries.SongInstanceID
      JOIN wsf.songbooks
      ON songbookentries.SongbookID = songbooks.SongbookID
      LEFT JOIN wsf.songbookvolumes
      ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID);
COMMIT;

-- Table of shared songs.
DROP TABLE IF EXISTS wsf_shiny.songbook_overlap;
CREATE TABLE wsf_shiny.songbook_overlap AS
(SELECT DISTINCT SongbookID1, SongbookName1, IncludeInSearch1,
        SongbookID2, SongbookName2, IncludeInSearch2, si1.SongID
 FROM ((SELECT SongbookID AS SongbookID1,
               SongbookName AS SongbookName1,
               IncludeInSearch AS IncludeInSearch1
        FROM wsf.songbooks) s1
       CROSS JOIN (SELECT SongbookID AS SongbookID2,
                          SongbookName AS SongbookName2,
                          IncludeInSearch AS IncludeInSearch2
                   FROM wsf.songbooks) s2)
      INNER JOIN (SELECT SongbookID, SongID
                  FROM songinstances_songbooks) si1
      ON SongbookID1 = si1.SongbookID
      INNER JOIN (SELECT SongbookID, SongID
                  FROM songinstances_songbooks) si2
      ON SongbookID2 = si2.SongbookID
         AND si1.SongID = si2.SongID
 WHERE SongbookID1 < SongbookID2);
COMMIT;

-- Table of arrangement types
DROP TABLE IF EXISTS wsf_shiny.arrangementtypes;
CREATE TABLE wsf_shiny.arrangementtypes AS
(SELECT ArrangementTypeID,
        ArrangementType
 FROM wsf.arrangementtypes);
COMMIT;

-- Table that connects song instances and arrangement types
DROP TABLE IF EXISTS wsf_shiny.songinstances_arrangementtypes;
CREATE TABLE wsf_shiny.songinstances_arrangementtypes AS
(SELECT DISTINCT SongInstanceID,
        SongID,
        ArrangementTypeID
 FROM wsf.songinstances
      JOIN wsf.arrangements_arrangementtypes
      ON songinstances.ArrangementID = arrangements_arrangementtypes.ArrangementID);
COMMIT;

-- Table of key signatures
DROP TABLE IF EXISTS wsf_shiny.keysignatures;
CREATE TABLE wsf_shiny.keysignatures AS
(SELECT KeySignatureID,
        PitchName,
        accidentals.AccidentalID,
        AccidentalSymbol,
        modes.ModeID,
        ModeName
 FROM wsf.keysignatures
      JOIN wsf.pitches
      ON keysignatures.PitchID = pitches.PitchID
      JOIN wsf.accidentals
      ON keysignatures.AccidentalID = accidentals.AccidentalID
      JOIN wsf.modes
      ON keysignatures.ModeID = modes.ModeID);
COMMIT;

-- Table that connects song instances and key signatures
DROP TABLE IF EXISTS wsf_shiny.songinstances_keysignatures;
CREATE TABLE wsf_shiny.songinstances_keysignatures AS
(SELECT songinstances.SongInstanceID,
        SongID, KeySignatureID
 FROM wsf.songinstances
      JOIN wsf.songinstances_keysignatures
      ON songinstances.SongInstanceID = songinstances_keysignatures.SongInstanceID);
COMMIT;

-- Table of time signatures
DROP TABLE IF EXISTS wsf_shiny.timesignatures;
CREATE TABLE wsf_shiny.timesignatures AS
(SELECT TimeSignatureID,
        TimeSignatureBeat,
        TimeSignatureMeasure
 FROM wsf.timesignatures);
COMMIT;

-- Table that connects song instances and time signatures
DROP TABLE IF EXISTS wsf_shiny.songinstances_timesignatures;
CREATE TABLE wsf_shiny.songinstances_timesignatures AS
(SELECT songinstances.SongInstanceID,
        SongID,
        TimeSignatureID
 FROM wsf.songinstances
      JOIN wsf.songinstances_timesignatures
      ON songinstances.SongInstanceID = songinstances_timesignatures.SongInstanceID);
COMMIT;

-- Table of meters
DROP TABLE IF EXISTS wsf_shiny.meters;
CREATE TABLE wsf_shiny.meters AS
(SELECT MeterID,
        Meter,
        Multiplier
 FROM wsf.meters);
COMMIT;

-- Table that connects song instances and meters
DROP TABLE IF EXISTS wsf_shiny.songinstances_meters;
CREATE TABLE wsf_shiny.songinstances_meters AS
(SELECT songinstances.SongInstanceID,
        SongID,
        MeterID
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_meters
      ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
 UNION DISTINCT
 SELECT songinstances.SongInstanceID, SongID, MeterID
 FROM wsf.songinstances
      JOIN wsf.songinstances_tunes
      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
      JOIN wsf.tunes_meters
      ON songinstances_tunes.TuneID = tunes_meters.TuneID);
COMMIT;

-- Table of lyrics first lines for all song instances
DROP TABLE IF EXISTS wsf_shiny.lyrics_first_lines;
CREATE TABLE wsf_shiny.lyrics_first_lines AS
(SELECT SongInstanceID,
        FirstLine,
        RefrainFirstLine
 FROM wsf.songinstances_lyrics
      JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID);
COMMIT;

-- Table of worship slots
DROP TABLE IF EXISTS wsf_shiny.worshipslots;
CREATE TABLE wsf_shiny.worshipslots AS
(SELECT WorshipSlotID,
        WorshipSlot,
        WorshipSlotOrder
 FROM wsf.worshipslots);
COMMIT;

-- Table that connects song instances and lyrics
DROP TABLE IF EXISTS wsf_shiny.songinstances_lyrics;
CREATE TABLE wsf_shiny.songinstances_lyrics AS
(SELECT SongInstanceID,
        LyricsID
 FROM wsf.songinstances_lyrics);
COMMIT;

-- Table that connects lyrics and translations
DROP TABLE IF EXISTS wsf_shiny.lyrics_translations;
CREATE TABLE wsf_shiny.lyrics_translations AS
(SELECT lyrics.LyricsID,
        TranslatedFromID,
        CASE WHEN lyrics.LanguageID = sources.LanguageID
                  THEN 'alt'
             ELSE 'tr'
        END AS Type
 FROM wsf.lyrics
      JOIN wsf.lyrics_translations
      ON lyrics.LyricsID = lyrics_translations.LyricsID
      JOIN wsf.lyrics sources
      ON lyrics_translations.TranslatedFromID = sources.LyricsID);
COMMIT;

-- Table that connects lyrics and artists
DROP TABLE IF EXISTS wsf_shiny.lyrics_artists;
CREATE TABLE wsf_shiny.lyrics_artists AS
(SELECT LyricsID, ArtistID
 FROM wsf.lyrics_artists);
COMMIT;

-- Table that connects lyrics and files
DROP TABLE IF EXISTS wsf_shiny.lyrics_files;
CREATE TABLE wsf_shiny.lyrics_files AS
(SELECT LyricsID, FileName
 FROM wsf.lyrics_files);
COMMIT;

-- Table of worship history
DROP TABLE IF EXISTS wsf_shiny.worshiphistory;
CREATE TABLE wsf_shiny.worshiphistory AS
(SELECT WorshipHistoryID,
        SongInstanceID,
        WorshipHistoryDate,
        WorshipSlotID
 FROM wsf.worshiphistory);
COMMIT;

-- Table of metrical psalms
DROP TABLE IF EXISTS wsf_shiny.metricalpsalms;
CREATE TABLE wsf_shiny.metricalpsalms AS
(SELECT metricalpsalms.MetricalPsalmID,
        metricalpsalms.PsalmNumber,
        metricalpsalms_prettyscripturelists.PrettyScriptureList
 FROM wsf.metricalpsalms
      LEFT JOIN wsf.metricalpsalms_prettyscripturelists
      ON metricalpsalms.MetricalPsalmID = metricalpsalms_prettyscripturelists.MetricalPsalmID);
COMMIT;

-- Table that connects psalm songs and lyrics
DROP TABLE IF EXISTS wsf_shiny.psalmsongs_lyrics;
CREATE TABLE wsf_shiny.psalmsongs_lyrics AS
(SELECT DISTINCT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
        lyrics.LyricsID, lyrics.FirstLine, lyrics.LanguageID,
        public_domain.PublicDomain,
        ROW_NUMBER() OVER (PARTITION BY psalmsongs.PsalmSongID,
                                        songinstances.SongInstanceID
                           ORDER BY lyrics.LyricsID) AS LyricsOrder
 FROM wsf.psalmsongs
      INNER JOIN wsf.songinstances
      ON psalmsongs.SongID = songinstances.SongID
      INNER JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      INNER JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID
      INNER JOIN (SELECT LyricsID,
                         CASE WHEN MIN(CopyrightHolderID) = 1
                                   THEN 'Y'
                              ELSE 'N'
                         END AS PublicDomain
                  FROM wsf.lyrics_copyrightholders
                  GROUP BY LyricsID) public_domain
      ON songinstances_lyrics.LyricsID = public_domain.LyricsID
 UNION ALL
 SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
		lyrics.LyricsID, lyrics.FirstLine, lyrics.LanguageID,
        public_domain.PublicDomain,
        ROW_NUMBER() OVER (PARTITION BY metricalpsalms.MetricalPsalmID
                           ORDER BY AvgVerse) AS LyricsOrder
 FROM wsf.metricalpsalms
      INNER JOIN wsf.metricalpsalms_lyrics
      ON metricalpsalms.MetricalPsalmID = metricalpsalms_lyrics.MetricalPsalmID
      INNER JOIN wsf.lyrics
      ON metricalpsalms_lyrics.LyricsID = lyrics.LyricsID
      INNER JOIN (SELECT LyricsID,
                         CASE WHEN MIN(CopyrightHolderID) = 1
                                   THEN 'Y'
                              ELSE 'N'
                         END AS PublicDomain
                  FROM wsf.lyrics_copyrightholders
                  GROUP BY LyricsID) public_domain
      ON lyrics.LyricsID = public_domain.LyricsID
      LEFT JOIN (SELECT lyrics_scripturereferences.LyricsID,
                        AVG(Verse) AS AvgVerse
                 FROM wsf.lyrics_scripturereferences
                      INNER JOIN wsf.scripturereferences
                      ON lyrics_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID
                 GROUP BY LyricsID) avg_verse
      ON metricalpsalms_lyrics.LyricsID = avg_verse.LyricsID
 WHERE metricalpsalms_lyrics.LyricsID NOT IN
       (SELECT LyricsID
        FROM wsf.songinstances_lyrics
             INNER JOIN wsf.songinstances
             ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID
             INNER JOIN wsf.psalmsongs
             ON songinstances.SongID = psalmsongs.SongID));
COMMIT;

-- Table of psalm songs
DROP TABLE IF EXISTS wsf_shiny.psalmsongs;
CREATE TABLE wsf_shiny.psalmsongs AS
(SELECT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
        PsalmNumber, psalmsongs.SongID, psalmsongs.PsalmSongTypeID,
        PsalmSongType, SongName AS PsalmSongTitle,
		PrettyScriptureList
 FROM wsf.psalmsongs
      LEFT JOIN wsf.psalmsongtypes
      ON psalmsongs.PsalmSongTypeID = psalmsongtypes.PsalmSongTypeID
      LEFT JOIN wsf.songs
      ON psalmsongs.SongID = songs.SongID
      LEFT JOIN wsf.psalmsongs_prettyscripturelists
      ON psalmsongs.PsalmSongID = psalmsongs_prettyscripturelists.PsalmSongID
 UNION ALL
 SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
        PsalmNumber, NULL AS SongID, NULL AS PsalmSongTypeID,
        'Paraphrase' AS PsalmSongType, FirstLine AS PsalmSongTitle,
        PrettyScriptureList
 FROM wsf.metricalpsalms
      INNER JOIN (SELECT PsalmSongID, FirstLine
                  FROM wsf_shiny.psalmsongs_lyrics
                  WHERE LyricsOrder = 1) not_psalmsongs
      ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = not_psalmsongs.PsalmSongID
	  LEFT JOIN wsf.metricalpsalms_prettyscripturelists
      ON metricalpsalms.MetricalPsalmID = metricalpsalms_prettyscripturelists.MetricalPsalmID);
COMMIT;

-- Table of psalm song types
DROP TABLE IF EXISTS wsf_shiny.psalmsongtypes;
CREATE TABLE wsf_shiny.psalmsongtypes AS
(SELECT PsalmSongTypeID, PsalmSongType
 FROM wsf.psalmsongtypes);
COMMIT;

-- Table that connects psalm songs and alternative tunes
DROP TABLE IF EXISTS wsf_shiny.psalmsongs_alternativetunes;
CREATE TABLE wsf_shiny.psalmsongs_alternativetunes
(PsalmSongID varchar(10),
 TuneID int,
 TuneDisplayName varchar(100),
 Notes varchar(1000));
INSERT INTO wsf_shiny.psalmsongs_alternativetunes
(SELECT PsalmSongID, tunes.TuneID,
        CONCAT(CASE WHEN tunes.RealTuneName = 1
                         THEN CONCAT(TuneName, ' (')
                    ELSE ''
               END,
               COALESCE(tunes.CanonicalSongName,
                        songs.SongName),
               CASE WHEN tunes.RealTuneName = 1
                         THEN ')'
                    ELSE ''
               END) AS TuneDisplayName,
        REPLACE(REPLACE(Notes, '#T#',
                        CONCAT('"',
                               COALESCE(tunes.CanonicalSongName,
                                        songs.SongName),
                               '"')),
                '#L#', CONCAT('"', PsalmSongTitle, '"')) AS Notes
 FROM wsf_shiny.psalmsongs
      INNER JOIN wsf.alternativetunes
      ON (PsalmSongID LIKE 'PS%'
          AND psalmsongs.SongID = alternativetunes.SongID)
         OR (PsalmSongID LIKE 'MP%'
             AND SUBSTRING(PsalmSongID, 3) = MetricalPsalmID)
      INNER JOIN wsf.tunes
      ON alternativetunes.TuneID = tunes.TuneID
      LEFT JOIN (SELECT TuneID, MAX(SongID) AS SongID
                 FROM wsf.tunes_canonicalsongs
                 GROUP BY TuneID
                 HAVING COUNT(*) = 1) one_canonical_song
      ON tunes.TuneID = one_canonical_song.TuneID
      LEFT JOIN wsf.songs
      ON one_canonical_song.SongID = songs.SongID);
COMMIT;

-- Table that connects tunes and canonical songs
DROP TABLE IF EXISTS wsf_shiny.tunes_canonicalsongs;
CREATE TABLE wsf_shiny.tunes_canonicalsongs AS
(SELECT TuneID, SongID
 FROM wsf.tunes_canonicalsongs);
COMMIT;