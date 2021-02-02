module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)

--Ohjelman alkutilanne
alkutilanne :: PeliTilanne 
alkutilanne =
    GameOn                                      -- pelitilanne alussa
      (Peli                                     -- peli
       0                                        -- Aika pelin aluss
       (10,0)                                   -- kopterin paikka
       (0,0)                                    -- mopeus
       0                                        -- teho
       0                                        -- kulma
       0                                        -- hemmoa
       [Talo 800 500 700]                       -- talon sijainti ja koko
       [Hemmo (700, 800), Hemmo (900, 800)]     -- hemmojen sijainti 
      )


main :: IO ()

--Main osio
main = play 
        (InWindow "Choplifter" (1000,600) (200,200))  -- ikkunan piirto
        (light blue)                                  -- taustaväri                
        24                                            -- Number of simulation steps to take for each second of real time  (suoraa haskell hackage )
        alkutilanne                                   -- pelimaailma
        piirräPeliTilanne                             -- piirtää pelitilanteen 
        reagoiPeliTilanne                             -- regoi pelissä tapahtuviin asioihin
        päivitäPelitilanne                            -- tuo peliin toimivuutta


-- pelitilanteeseen reagoiminen ja restart mahdollisuus
-- tarkastellaan pelitilannetta onko peli päällä vai ei
reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne 
    = case pelitilanne of      
        GameOn cl -> GameOn (reagoi tapahtuma cl)   -- jos peli on päällä kutsutaan reagoi funktiota tapahtumalla ja pelillä
        GameOver cl -> case tapahtuma of            -- jos peli on ei ole päällä tarkastetaan tuleeko tapahtumaa missä painetaan r
                         EventKey (Char 'r') Down _ _   -> alkutilanne   -- jos r:ää painetaan palautetaan alkutilanne
                         _ -> GameOver cl


-- reagointi 
-- tarkastellaan mikä tapahtuma on kyseessä ja toimitaan sen mukaan
reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli 
    = case tapahtuma of
        EventKey (Char 'w') Down _ _ -> muutaTehoa 2.5 peli
        EventKey (Char 's') Down _ _ -> muutaTehoa (-2.5) peli
        EventKey (Char 'a') Down _ _ -> kallista (-8) peli
        EventKey (Char 'd') Down _ _ -> kallista (8) peli
        _ -> peli
    

-- katsotaan onko kopteri törmännyt mihinkän 
päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne 
    = case pelitilanne of
        GameOver cl -> GameOver cl
        GameOn cl   -> case  törmääköTaloon (cl_paikka cl) (cl_kulma cl) (cl_talot cl) of -- kutsutaan törmääkö taloon funktiota joka palauttaa onko törmännyt 
                        Nothing -> GameOn (päivitäPeliä aikaEdellisestä cl)               -- ei ole törmännyt joten päivitetään aikaa edellisestä päivyksestä tähän aikaa 
                        Just Roottori -> GameOver cl                                      -- roottori törmännyt joten peli loppui
                        Just Laskuteline                                                  -- tarkastelee onko laskutelineelle tullutlasku riittävän hidas
                            | onkoHyväLaskeutuminen (cl_nopeus cl) (cl_kulma cl)
                                -> GameOn (päivitäPeliä aikaEdellisestä 
                                            cl{cl_kulma=0
                                              ,cl_nopeus=pysäytäPystyssä (cl_nopeus cl)})
                            | otherwise -> GameOver cl


-- laittaa kopterin suoraan jos on vähän vinossa laskeutunut
pysäytäPystyssä :: Vector -> Vector
pysäytäPystyssä (vx,vy) = (vx, max 0 vy)


-- tarkistetaan onko laskeutuminen ollut riittävän hidas ja oikeassa kulmaassa
onkoHyväLaskeutuminen :: Vector -> Float -> Bool
onkoHyväLaskeutuminen nopeus kulma
    | magV nopeus < 80 && abs kulma <= 10 = True
    | otherwise = False


-- päivittää kopterin liiketta ja tapahtumaa pelikentällä 
päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
     Peli aika (kopteriX,kopteriY) 
               (vX,vY) 
               teho kulma
               hemmojaKyydissä
               talot
               hemmot
        -> let
            (dX,dY) = kulmaJaTehoKiihtyvyydeksi teho kulma  
            nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- (kopteriX,kopteriY)) < 50 -- hemmo nousee kyytiin 
            (hemmotKopteriin,hemmotUlkona) = partition nouseekoKyytiin hemmot               -- jos on riittävän lähellä kopteria 
           in Peli (aika + aikaEdellisestä)                                -- uudet päivitettät tilanteet
                   (kopteriX+ aikaEdellisestä *  vX                        -- uusi sijainti
                   , max 0 (kopteriY+aikaEdellisestä *  vY) )
                   ((vX + dX) * 0.97 , (vY + dY - 5) * 0.97 )
                   teho
                   kulma
                   (hemmojaKyydissä + genericLength hemmotKopteriin)        -- jos hemmoja on otettu kyytiin
                   talot
                   (map (päivitäHemmoa edellinenTila) hemmotUlkona)         -- hemmoja otettu kyytiin niin hemmot pois katolta

-- laittaa kopterin menemään oikeaan suuntaa oikealla nopeudella
kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float,Float)
kulmaJaTehoKiihtyvyydeksi teho kulma 
    = rotateV (- degToRad kulma) (0,teho) 


-- kopterin ylä ja ala puolelle tehdyt näkymätömät viiva minkä perusteella 
-- kopteri lasketaan törmänneeksi taloon
kopteriTörmäysviivat :: Point -> Float -> ((Point,Point) , (Point,Point))
kopteriTörmäysviivat paikka kulma = 
    let
     vasen = -170
     oikea = 100 
     kääntö = rotateV (- degToRad kulma)   -- pyörii kopterin mukaan
    in (  (kääntö (vasen,0) #+ paikka
          ,kääntö (oikea,0) #+ paikka)
          ,
          (kääntö (vasen,120) #+ paikka
          ,kääntö (oikea,120) #+ paikka)
       )


-- törmäyskohta on joko laskuteline tai roottori
data TörmäysKohta = Laskuteline | Roottori 
        deriving (Eq,Ord,Show)


-- katsotaan törmääkö kopteri taloon vai ei
törmääköTaloon :: Point -> Float -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon paikka kulma talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
                                   -- case (nonEmpty (mapMaybe törmääköYhteen talot)) of
                                   --  Nothing -> Nothing
                                   --  Just kohdat -> Just (maximum1 kohdat)
    where
        -- tarkastetaan osuuko talon reunat ja kopterin törmäysviivat yhteen
     törmääköYhteen talo 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = kopteriTörmäysviivat paikka kulma
            (va,oy)   = nurkkaPisteet talo 
          in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
                (True,False) -> Just Laskuteline
                (False,False) -> Nothing
                _ -> Just Roottori
          


-- piirtää kuvan pelitilanteen perusteella 
piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne 
    = case pelitilanne of
        GameOver cl -> piirräPeli cl <> translate 0 0 (scale 0.4 0.4 (color black (text "GAME OVER")))
                                     <> translate 0 (-100) (scale 0.4 0.4 (color black (text "Press r to restart")))
        GameOn cl   -> piirräPeli cl


-- piirtää pelin tiedot kuviksi
piirräPeli :: Choplifter -> Picture
piirräPeli peli = let
                   kulma = cl_kulma peli 
                   aika  = cl_aika peli
                   (kopteriX,kopteriY) = cl_paikka peli
                   teho = cl_teho peli
                   talot = cl_talot peli
                   kopterikuva = rotate kulma (scale 0.4 0.4 (kopteri teho aika))
                   hemmoKuvat = map (piirräHemmo aika)  (cl_hemmot peli)
                   taloKuvat  = map piirräTalo talot
                   peliKuva = translate kopteriX kopteriY kopterikuva 
                                        <> maa  
                                        <> pictures taloKuvat
                                        <> pictures hemmoKuvat

                                        
                  in scale 0.25 0.25 (translate 0 (-180) peliKuva)


-- kopterin kallistaminen 
kallista :: Float -> Choplifter -> Choplifter
kallista muutos peli = peli{cl_kulma = muutos + cl_kulma peli}


-- kopterin tehon muutos
muutaTehoa :: Float -> Choplifter -> Choplifter
muutaTehoa muutos peli = peli{cl_teho = muutos + cl_teho peli}
                                          ---       ↑
                                          --     cl_teho :: Choplifter -> Float
                          

-- pelitilanne on joko gameOver ja gameOn
data PeliTilanne = GameOver Choplifter | GameOn Choplifter


-- pelin tiedot 
data Choplifter 
 = Peli 
   {
     cl_aika   :: Float          -- ^ Aika pelin alusta
    ,cl_paikka :: (Float, Float) -- ^ Missä kopteri?
    ,cl_nopeus :: (Float, Float) -- ^ Kuinka nopeasti menee?
    ,cl_teho   :: Float          -- ^ Teho
    ,cl_kulma  :: Float          -- ^ Kuinka vinossa
    ,cl_hemmojaKyydissä :: Natural -- Kuinka monta hemmoa kerätty 
   
    ,cl_talot  :: [Talo]         -- Esteet pelissä
    ,cl_hemmot :: [Hemmo]        -- Pelihahmot
    
   }


-- talon korkeus
korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map osuukoTaloon . cl_talot $ peli
 where
  osuukoTaloon :: Talo -> Float
  osuukoTaloon talo
    | abs (talo_sijainti talo - kohta) < (talo_leveys talo / 2) = talo_korkeus
      talo
    | otherwise = 0



-- Hemmot  
data Hemmo = Hemmo {hemmo_sijainti :: Point}

-- laittaa hemmon liikkumaan 
haluaakoLiikkua :: Choplifter -> Hemmo -> Bool
haluaakoLiikkua peli hemmo = haluaaLiikkua && not putoaako
     where
        kopterinPaikka = cl_paikka peli
        putoaako = abs (korkeusEdessä - snd (hemmo_sijainti hemmo)) > 50
        korkeusEdessä = korkeusKohdassa (fst (hemmo_sijainti hemmo) + suunta * 2)
                                        peli  
        haluaaLiikkua = magV (kopterinPaikka #- hemmo_sijainti hemmo) < 600
        suunta = minneHemmoMenisi peli hemmo


-- hemmo lähtee kohti kopteria 
minneHemmoMenisi :: Choplifter -> Hemmo -> Float
minneHemmoMenisi peli hemmo
            | fst kopterinPaikka < fst (hemmo_sijainti hemmo)  
                = -15
            | otherwise             
                =  15
     where
        kopterinPaikka = cl_paikka peli


-- päivittää hemmon sijaintia 
päivitäHemmoa :: Choplifter -> Hemmo -> Hemmo
päivitäHemmoa peli hemmo 
        | haluaakoLiikkua peli hemmo 
            = hemmo{hemmo_sijainti = hemmo_sijainti hemmo #+ (suunta,0)}
        | otherwise 
            = hemmo
    where   
     suunta = minneHemmoMenisi peli hemmo


-- piirtää hemmon pelialustalle
piirräHemmo :: Float -> Hemmo -> Picture
piirräHemmo aika hemmo = let 
                     (x,y) = hemmo_sijainti hemmo
                     lantio = (15,40)
                     vasenJalka = 15+sin (12*aika) * 7
                     oikeaJalka = 15+cos (12*aika) * 7
                     hemmonKuva = color white 
                        (translate 0 110 (circleSolid 20)
                          <> line [(0,100), lantio] -- selkä
                          <> line [(-40,90 + cos (8*aika+0.3) * 40),(-30,90), (30,90)
                                  , (40,90 + cos (8*aika) * 40)] -- kädet
                          <> line [(-25,vasenJalka), (-20,vasenJalka) 
                                  , lantio
                                  , (30,oikeaJalka), (35,oikeaJalka)] --jalat
                        )
                    in translate x y hemmonKuva



--- Talot
data Talo = Talo {talo_korkeus :: Float, talo_leveys :: Float
                 ,talo_sijainti :: Float }

-- piirtää talon pelialustalle                
piirräTalo :: Talo -> Picture
piirräTalo talo = let
                   paikoillaan = translate (talo_sijainti talo) (talo_korkeus talo / 2) talonKuva
                   talonKuva = color (greyN 0.5) 
                                (rectangleSolid (talo_leveys talo) (talo_korkeus talo))
                   ((vax,vay),(oyx,oyy)) = nurkkaPisteet talo 
                   apupisteet =  translate vax vay (color red (circleSolid 10))
                                <> translate oyx oyy (color red (circleSolid 10))
                  in paikoillaan <> apupisteet


-- talon nurkissa olevat pisteet
nurkkaPisteet :: Talo -> (Point,Point)
nurkkaPisteet talo = 
    let
        vasenAla = (talo_sijainti talo - (talo_leveys talo / 2) , 0)
        oikeaYlä = (talo_sijainti talo + (talo_leveys talo / 2)      , talo_korkeus talo) 
    in (vasenAla,oikeaYlä)

-- vihreä maa 
maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))


-- kopterin piirtäminen
kopteri :: Float -> Float -> Picture
kopteri teho aika = translate 0 (150) (color white runko)
 where
  runko = circleSolid 100 
            <> translate (-200) 0 (rectangleSolid 300 30) -- palkki rungosta perään 
            <> translate (-350) 0 (rectangleSolid 30 80) -- palkki rungon perässä 
            <> lapa                                       -- lapa   
            <> translate 0 90            (rectangleSolid 10 120) -- lavan kiinnityspalkki
            <> translate (-50) (-90)     (rectangleSolid 10 120) -- laskuteline palkki oikea
            <> translate (50) (-90)      (rectangleSolid 10 120) -- laskuteline palkki vasen
            <> translate 0 (-150)        (rectangleSolid 200 15) -- laskutelinepalkki   
            <> peraLapa                                   -- perä lapa 

  lapa = translate 0 150 (rectangleSolid (350 * sin (aika * teho)) 10)
  peraLapa = translate (-350) 40 (rectangleSolid (150 * sin (aika * teho)) 10)

{-
--- Puu
data Puu = Puu {puu_korkeus :: Float, puu_leveys :: Float
                 ,puu_sijainti :: Float }

piirräPuu :: Puu -> Picture
piirräPuu puu = let
                   paikoillaan = translate (puu_sijainti puu) (puu_korkeus puu / 2) talonKuva

                   talonKuva = color (greyN 0.5) 
                                (rectangleSolid (puu_leveys puu) (puu_korkeus puu))

                   ((vax,vay),(oyx,oyy)) = puunNurkkaPisteet puu 

                   apupisteet =  translate vax vay (color red (circleSolid 10))
                                <> translate oyx oyy (color red (circleSolid 10))
                  in paikoillaan <> apupisteet

-- type Point = (Float,Float)
puunNurkkaPisteet :: Puu -> (Point,Point)
puunNurkkaPisteet puu = 
    let
        vasenAla = (puu_sijainti puu - (puu_leveys puu / 2) , 0)
        oikeaYlä = (puu_sijainti puu + (puu_leveys puu / 2)      , puu_korkeus puu) 
    in (vasenAla,oikeaYlä)
-}
--
(#+) :: Point -> Vector -> Point
(a,b) #+ (x,y) = (a+x,b+y)
(#-) :: Point -> Point -> Vector
(a,b) #- (x,y) = (a-x,b-y)