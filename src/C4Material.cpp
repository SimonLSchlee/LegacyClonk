/*
 * LegacyClonk
 *
 * Copyright (c) 1998-2000, Matthes Bender (RedWolf Design)
 * Copyright (c) 2017-2019, The LegacyClonk Team and contributors
 *
 * Distributed under the terms of the ISC license; see accompanying file
 * "COPYING" for details.
 *
 * "Clonk" is a registered trademark of Matthes Bender, used with permission.
 * See accompanying file "TRADEMARK" for details.
 *
 * To redistribute this file separately, substitute the full license texts
 * for the above references.
 */

/* Material definitions used by the landscape */

#include <C4Include.h>
#include <C4Material.h>
#include <C4Components.h>

#include <C4Group.h>
#include <C4Game.h>
#include <C4Random.h>
#include <C4ToolsDlg.h> // For C4TLS_MatSky...
#ifdef C4ENGINE
#include <C4Wrappers.h>
#include <C4Physics.h> // For GravAccel
#endif

// C4MaterialReaction

#ifdef C4ENGINE

struct ReactionFuncMapEntry { const char *szRFName; C4MaterialReactionFunc pFunc; };

const ReactionFuncMapEntry ReactionFuncMap[] =
{
	{ "Script",  &C4MaterialMap::mrfScript },
	{ "Convert", &C4MaterialMap::mrfConvert },
	{ "Poof",    &C4MaterialMap::mrfPoof },
	{ "Corrode", &C4MaterialMap::mrfCorrode },
	{ "Insert",  &C4MaterialMap::mrfInsert },
	{ nullptr, &C4MaterialReaction::NoReaction }
};

#endif

void C4MaterialReaction::CompileFunc(StdCompiler *pComp)
{
#ifdef C4ENGINE
	if (pComp->isCompiler()) pScriptFunc = nullptr;
	// compile reaction func ptr
	StdStrBuf sReactionFuncName;
	int32_t i = 0; while (ReactionFuncMap[i].szRFName && (ReactionFuncMap[i].pFunc != pFunc)) ++i;
	sReactionFuncName.Ref(ReactionFuncMap[i].szRFName);
	pComp->Value(mkNamingAdapt(sReactionFuncName, "Type", StdStrBuf()));
	i = 0; while (ReactionFuncMap[i].szRFName && !SEqual(ReactionFuncMap[i].szRFName, sReactionFuncName.getData())) ++i;
	pFunc = ReactionFuncMap[i].pFunc;
#endif
	// compile the rest
	pComp->Value(mkNamingAdapt(TargetSpec,      "TargetSpec",    StdStrBuf()));
	pComp->Value(mkNamingAdapt(ScriptFunc,      "ScriptFunc",    StdStrBuf()));
	pComp->Value(mkNamingAdapt(iExecMask,       "ExecMask",      ~0u));
	pComp->Value(mkNamingAdapt(fReverse,        "Reverse",       false));
	pComp->Value(mkNamingAdapt(fInverseSpec,    "InverseSpec",   false));
	pComp->Value(mkNamingAdapt(fInsertionCheck, "CheckSlide",    true));
	pComp->Value(mkNamingAdapt(iDepth,          "Depth",         0));
	pComp->Value(mkNamingAdapt(sConvertMat,     "ConvertMat",    StdStrBuf()));
	pComp->Value(mkNamingAdapt(iCorrosionRate,  "CorrosionRate", 100));
}

#ifdef C4ENGINE
void C4MaterialReaction::ResolveScriptFuncs(const char *szMatName)
{
	// get script func for script-defined behaviour
	if (pFunc == &C4MaterialMap::mrfScript)
		pScriptFunc = Game.ScriptEngine.GetSFuncWarn(this->ScriptFunc.getData(), AA_PROTECTED, FormatString("Material reaction of \"%s\"", szMatName).getData());
	else
		pScriptFunc = nullptr;
}
#endif

// C4MaterialCore

C4MaterialCore::C4MaterialCore()
{
	Clear();
}

void C4MaterialCore::Clear()
{
	CustomReactionList.clear();
	sTextureOverlay.Clear();
	sPXSGfx.Clear();
	sBlastShiftTo.Clear();
	sInMatConvert.Clear();
	sInMatConvertTo.Clear();
	sBelowTempConvertTo.Clear();
	sAboveTempConvertTo.Clear();
	*Name = '\0';
	int32_t i;
	for (i = 0; i < C4M_ColsPerMat * 3; ++i) Color[i] = 0;
	for (i = 0; i < C4M_ColsPerMat * 2; ++i) Alpha[i] = 0;
	MapChunkType = 0;
	Density = 0;
	Friction = 0;
	DigFree = 0;
	BlastFree = 0;
	Dig2Object = 0;
	Dig2ObjectRatio = 0;
	Dig2ObjectOnRequestOnly = 0;
	Blast2Object = 0;
	Blast2ObjectRatio = 0;
	Blast2PXSRatio = 0;
	Instable = 0;
	MaxAirSpeed = 0;
	MaxSlide = 0;
	WindDrift = 0;
	Inflammable = 0;
	Incindiary = 0;
	Extinguisher = 0;
	Corrosive = 0;
	Corrode = 0;
	Soil = 0;
	Placement = 0;
	OverlayType = 0;
	PXSGfxRt.Default();
	PXSGfxSize = 0;
	InMatConvertDepth = 0;
	BelowTempConvert = 0;
	BelowTempConvertDir = 0;
	AboveTempConvert = 0;
	AboveTempConvertDir = 0;
	ColorAnimation = 0;
	TempConvStrength = 0;
	MinHeightCount = 0;
	SplashRate = 10;
}

bool C4MaterialCore::Load(CppC4Group &group,
	const std::string &entryPath)
{
	if (StdStrBuf Source; !(CppC4Group_LoadEntryString(group, entryPath, Source) && CompileFromBuf_LogWarn<StdCompilerINIRead>(*this, Source, entryPath.c_str())))
	{
		return false;
	}

	// adjust placement, if not specified
#ifdef C4ENGINE
	if (!Placement)
	{
		if (DensitySolid(Density))
		{
			Placement = 30;
			if (!DigFree) Placement += 20;
			if (!BlastFree) Placement += 10;
			if (!Dig2ObjectOnRequestOnly) Placement += 10;
		}
		else if (DensityLiquid(Density))
			Placement = 10;
		else Placement = 5;
	}
#endif
	return true;
}

uint32_t C4MaterialCore::GetDWordColor(int32_t iIndex)
{
	if (iIndex < 0) return 0;
	iIndex %= (C4M_ColsPerMat * 2);
	int32_t iClrIndex = iIndex % C4M_ColsPerMat;
	return RGB(Color[iClrIndex * 3 + 2], Color[iClrIndex * 3 + 1], Color[iClrIndex * 3]) | (Alpha[iIndex] << 24);
}

void C4MaterialCore::CompileFunc(StdCompiler *pComp)
{
	if (pComp->isCompiler()) Clear();
	pComp->Name("Material");
	pComp->Value(mkNamingAdapt(toC4CStr(Name),                                                   "Name",                ""));
	pComp->Value(mkNamingAdapt(toC4CArrU(Color),                                                 "Color"));
	pComp->Value(mkNamingAdapt(toC4CArrU(Color),                                                 "ColorX",              Color));
	pComp->Value(mkNamingAdapt(toC4CArrU(Alpha),                                                 "Alpha"));
	pComp->Value(mkNamingAdapt(ColorAnimation,                                                   "ColorAnimation",      0));
	pComp->Value(mkNamingAdapt(MapChunkType,                                                     "Shape",               0));
	pComp->Value(mkNamingAdapt(Density,                                                          "Density",             0));
	pComp->Value(mkNamingAdapt(Friction,                                                         "Friction",            0));
	pComp->Value(mkNamingAdapt(DigFree,                                                          "DigFree",             0));
	pComp->Value(mkNamingAdapt(BlastFree,                                                        "BlastFree",           0));
	pComp->Value(mkNamingAdapt(mkC4IDAdapt(Blast2Object),                                        "Blast2Object",        0));
	pComp->Value(mkNamingAdapt(mkC4IDAdapt(Dig2Object),                                          "Dig2Object",          0));
	pComp->Value(mkNamingAdapt(Dig2ObjectRatio,                                                  "Dig2ObjectRatio",     0));
	pComp->Value(mkNamingAdapt(Dig2ObjectOnRequestOnly,                                          "Dig2ObjectRequest",   0));
	pComp->Value(mkNamingAdapt(Blast2ObjectRatio,                                                "Blast2ObjectRatio",   0));
	pComp->Value(mkNamingAdapt(Blast2PXSRatio,                                                   "Blast2PXSRatio",      0));
	pComp->Value(mkNamingAdapt(Instable,                                                         "Instable",            0));
	pComp->Value(mkNamingAdapt(MaxAirSpeed,                                                      "MaxAirSpeed",         0));
	pComp->Value(mkNamingAdapt(MaxSlide,                                                         "MaxSlide",            0));
	pComp->Value(mkNamingAdapt(WindDrift,                                                        "WindDrift",           0));
	pComp->Value(mkNamingAdapt(Inflammable,                                                      "Inflammable",         0));
	pComp->Value(mkNamingAdapt(Incindiary,                                                       "Incindiary",          0));
	pComp->Value(mkNamingAdapt(Corrode,                                                          "Corrode",             0));
	pComp->Value(mkNamingAdapt(Corrosive,                                                        "Corrosive",           0));
	pComp->Value(mkNamingAdapt(Extinguisher,                                                     "Extinguisher",        0));
	pComp->Value(mkNamingAdapt(Soil,                                                             "Soil",                0));
	pComp->Value(mkNamingAdapt(Placement,                                                        "Placement",           0));
	pComp->Value(mkNamingAdapt(mkParAdapt(sTextureOverlay, StdCompiler::RCT_IdtfAllowEmpty),     "TextureOverlay",      ""));
	pComp->Value(mkNamingAdapt(OverlayType,                                                      "OverlayType",         0));
	pComp->Value(mkNamingAdapt(mkParAdapt(sPXSGfx, StdCompiler::RCT_IdtfAllowEmpty),             "PXSGfx",              ""));
	pComp->Value(mkNamingAdapt(PXSGfxRt,                                                         "PXSGfxRt",            TargetRect0));
	pComp->Value(mkNamingAdapt(PXSGfxSize,                                                       "PXSGfxSize",          PXSGfxRt.Wdt));
	pComp->Value(mkNamingAdapt(TempConvStrength,                                                 "TempConvStrength",    0));
	pComp->Value(mkNamingAdapt(mkParAdapt(sBlastShiftTo, StdCompiler::RCT_IdtfAllowEmpty),       "BlastShiftTo",        ""));
	pComp->Value(mkNamingAdapt(mkParAdapt(sInMatConvert, StdCompiler::RCT_IdtfAllowEmpty),       "InMatConvert",        ""));
	pComp->Value(mkNamingAdapt(mkParAdapt(sInMatConvertTo, StdCompiler::RCT_IdtfAllowEmpty),     "InMatConvertTo",      ""));
	pComp->Value(mkNamingAdapt(InMatConvertDepth,                                                "InMatConvertDepth",   0));
	pComp->Value(mkNamingAdapt(AboveTempConvert,                                                 "AboveTempConvert",    0));
	pComp->Value(mkNamingAdapt(AboveTempConvertDir,                                              "AboveTempConvertDir", 0));
	pComp->Value(mkNamingAdapt(mkParAdapt(sAboveTempConvertTo, StdCompiler::RCT_IdtfAllowEmpty), "AboveTempConvertTo",  ""));
	pComp->Value(mkNamingAdapt(BelowTempConvert,                                                 "BelowTempConvert",    0));
	pComp->Value(mkNamingAdapt(BelowTempConvertDir,                                              "BelowTempConvertDir", 0));
	pComp->Value(mkNamingAdapt(mkParAdapt(sBelowTempConvertTo, StdCompiler::RCT_IdtfAllowEmpty), "BelowTempConvertTo",  ""));
	pComp->Value(mkNamingAdapt(MinHeightCount,                                                   "MinHeightCount",      0));
	pComp->Value(mkNamingAdapt(SplashRate,                                                       "SplashRate",          10));
	pComp->NameEnd();
	// material reactions
	pComp->Value(mkNamingAdapt(
		mkSTLContainerAdapt(CustomReactionList),
		"Reaction", std::vector<C4MaterialReaction>()));
}

// C4Material

C4Material::C4Material()
{
	BlastShiftTo = 0;
	InMatConvertTo = MNone;
	BelowTempConvertTo = 0;
	AboveTempConvertTo = 0;
}

#ifdef C4ENGINE
void C4Material::UpdateScriptPointers()
{
	for (uint32_t i = 0; i < CustomReactionList.size(); ++i)
		CustomReactionList[i].ResolveScriptFuncs(Name);
}
#endif

// C4MaterialMap

#ifdef C4ENGINE

C4MaterialMap::C4MaterialMap() : DefReactConvert(&mrfConvert), DefReactPoof(&mrfPoof), DefReactCorrode(&mrfCorrode), DefReactIncinerate(&mrfIncinerate), DefReactInsert(&mrfInsert)
{
	Default();
}

#else

C4MaterialMap::C4MaterialMap()
{
	Default();
}

#endif

C4MaterialMap::~C4MaterialMap()
{
	Clear();
}

void C4MaterialMap::Clear()
{
	delete[] Map;           Map           = nullptr;
	delete[] ppReactionMap; ppReactionMap = nullptr;
}

int32_t C4MaterialMap::Load(CppC4Group &group, CppC4Group *overloadFile)
{
	size_t matCount = 0;
	CppC4Group_ForEachEntryByWildcard(group, "", C4CFN_MaterialFiles, [&matCount](const auto &) { ++matCount; return true; });

	// Allocate new map
	C4Material *pNewMap = new C4Material[matCount + Num];
	if (!pNewMap) return 0;

	// Load material cores to map
	size_t count = 0;
	CppC4Group_ForEachEntryByWildcard(group, "", C4CFN_MaterialFiles, [&group, &count, &pNewMap, this](const auto &info)
	{
		if (!pNewMap[count].Load(group, info.fileName))
		{
			delete[] pNewMap;
			count = 0;
			return false;
		}

		if (Get(pNewMap[count].Name) == MNone)
		{
			++count;
		}

		return true;
	});

	// Take over old materials.
	for (int32_t i = 0; i < Num; i++)
	{
		pNewMap[count + i] = Map[i];
	}
	delete[] Map;
	Map = pNewMap;

	// set material number
	Num += count;

	return count;
}

int32_t C4MaterialMap::Get(const char *szMaterial)
{
	int32_t cnt;
	for (cnt = 0; cnt < Num; cnt++)
		if (SEqualNoCase(szMaterial, Map[cnt].Name))
			return cnt;
	return MNone;
}

#ifdef C4ENGINE

void C4MaterialMap::CrossMapMaterials() // Called after load
{
	// build reaction function map
	delete[] ppReactionMap;
	typedef C4MaterialReaction *C4MaterialReactionPtr;
	ppReactionMap = new C4MaterialReactionPtr[(Num + 1) * (Num + 1)];
	for (int32_t iMatPXS = -1; iMatPXS < Num; iMatPXS++)
	{
		C4Material *pMatPXS = (iMatPXS + 1) ? Map + iMatPXS : nullptr;
		for (int32_t iMatLS = -1; iMatLS < Num; iMatLS++)
		{
			C4MaterialReaction *pReaction = nullptr;
			C4Material *pMatLS = (iMatLS + 1) ? Map + iMatLS : nullptr;
			// natural stuff: material conversion here?
			if (pMatPXS && pMatPXS->sInMatConvert.getLength() && SEqualNoCase(pMatPXS->sInMatConvert.getData(), pMatLS ? pMatLS->Name : C4TLS_MatSky))
				pReaction = &DefReactConvert;
			// the rest is happening for same/higher densities only
			else if ((MatDensity(iMatPXS) <= MatDensity(iMatLS)) && pMatPXS && pMatLS)
			{
				// incindiary vs extinguisher
				if ((pMatPXS->Incindiary && pMatLS->Extinguisher) || (pMatPXS->Extinguisher && pMatLS->Incindiary))
					pReaction = &DefReactPoof;
				// incindiary vs inflammable
				else if ((pMatPXS->Incindiary && pMatLS->Inflammable) || (pMatPXS->Inflammable && pMatLS->Incindiary))
					pReaction = &DefReactIncinerate;
				// corrosive vs corrode
				else if (pMatPXS->Corrosive && pMatLS->Corrode)
					pReaction = &DefReactCorrode;
				// otherwise, when hitting same or higher density: Material insertion
				else
					pReaction = &DefReactInsert;
			}
			// assign the function; or nullptr for no reaction
			SetMatReaction(iMatPXS, iMatLS, pReaction);
		}
	}
	// material-specific initialization
	int32_t cnt;
	for (cnt = 0; cnt < Num; cnt++)
	{
		C4Material *pMat = Map + cnt;
		const char *szTextureOverlay = nullptr;
		// newgfx: init pattern
		if (Map[cnt].sTextureOverlay.getLength())
			if (Game.TextureMap.GetTexture(Map[cnt].sTextureOverlay.getLength()))
			{
				szTextureOverlay = Map[cnt].sTextureOverlay.getData();
				// backwards compatibility: if a pattern was specified although the no-pattern flag was set, overwrite that flag
				if (Map[cnt].OverlayType & C4MatOv_None)
				{
					DebugLogF("Error in overlay of material %s: Flag C4MatOv_None ignored because a custom overlay (%s) was specified!", Map[cnt].Name, szTextureOverlay);
					Map[cnt].OverlayType &= ~C4MatOv_None;
				}
			}
		// default to smooth
		if (!szTextureOverlay)
			szTextureOverlay = "Smooth";
		// search/create entry in texmap
		Map[cnt].DefaultMatTex = Game.TextureMap.GetIndex(Map[cnt].Name, szTextureOverlay, true,
			FormatString("DefaultMatTex of mat %s", Map[cnt].Name).getData());
		const C4TexMapEntry *pTex = Game.TextureMap.GetEntry(Map[cnt].DefaultMatTex);
		if (pTex)
		{
			// take pattern
			Map[cnt].MatPattern = pTex->getPattern();
			// special zooming for overlay
			Map[cnt].MatPattern.SetZoom((Map[cnt].OverlayType & C4MatOv_Exact) ? 1 : 2);
		}
		// init PXS facet
		CSurface *sfcTexture;
		C4Texture *Texture;
		if (Map[cnt].sPXSGfx.getLength())
			if (Texture = Game.TextureMap.GetTexture(Map[cnt].sPXSGfx.getData()))
				if (sfcTexture = Texture->Surface32)
					Map[cnt].PXSFace.Set(sfcTexture, Map[cnt].PXSGfxRt.x, Map[cnt].PXSGfxRt.y, Map[cnt].PXSGfxRt.Wdt, Map[cnt].PXSGfxRt.Hgt);
		// evaluate reactions for that material
		for (unsigned int iRCnt = 0; iRCnt < pMat->CustomReactionList.size(); ++iRCnt)
		{
			C4MaterialReaction *pReact = &(pMat->CustomReactionList[iRCnt]);
			if (pReact->sConvertMat.getLength()) pReact->iConvertMat = Get(pReact->sConvertMat.getData()); else pReact->iConvertMat = -1;
			// evaluate target spec
			int32_t tmat;
			if (MatValid(tmat = Get(pReact->TargetSpec.getData())))
			{
				// single material target
				if (pReact->fInverseSpec)
					for (int32_t cnt2 = -1; cnt2 < Num; cnt2++) if (cnt2 != tmat) SetMatReaction(cnt, cnt2, pReact);
					else
						SetMatReaction(cnt, tmat, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "All"))
			{
				// add to all materials, including sky
				if (!pReact->fInverseSpec) for (int32_t cnt2 = -1; cnt2 < Num; cnt2++) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Solid"))
			{
				// add to all solid materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (DensitySolid(Map[cnt2].Density) != pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "SemiSolid"))
			{
				// add to all semisolid materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (DensitySemiSolid(Map[cnt2].Density) != pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Background"))
			{
				// add to all BG materials, including sky
				if (!pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (!Map[cnt2].Density != pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Sky"))
			{
				// add to sky
				if (!pReact->fInverseSpec)
					SetMatReaction(cnt, -1, pReact);
				else
					for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Incindiary"))
			{
				// add to all incendiary materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (!Map[cnt2].Incindiary == pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Extinguisher"))
			{
				// add to all incendiary materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (!Map[cnt2].Extinguisher == pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Inflammable"))
			{
				// add to all incendiary materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (!Map[cnt2].Inflammable == pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Corrosive"))
			{
				// add to all incendiary materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (!Map[cnt2].Corrosive == pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
			else if (SEqualNoCase(pReact->TargetSpec.getData(), "Corrode"))
			{
				// add to all incendiary materials
				if (pReact->fInverseSpec) SetMatReaction(cnt, -1, pReact);
				for (int32_t cnt2 = 0; cnt2 < Num; cnt2++) if (!Map[cnt2].Corrode == pReact->fInverseSpec) SetMatReaction(cnt, cnt2, pReact);
			}
		}
	}
	// second loop (DefaultMatTex is needed by GetIndexMatTex)
	for (cnt = 0; cnt < Num; cnt++)
	{
		if (Map[cnt].sBlastShiftTo.getLength())
			Map[cnt].BlastShiftTo = Game.TextureMap.GetIndexMatTex(Map[cnt].sBlastShiftTo.getData(), nullptr, true, FormatString("BlastShiftTo of mat %s", Map[cnt].Name).getData());
		if (Map[cnt].sInMatConvertTo.getLength())
			Map[cnt].InMatConvertTo = Get(Map[cnt].sInMatConvertTo.getData());
		if (Map[cnt].sBelowTempConvertTo.getLength())
			Map[cnt].BelowTempConvertTo = Game.TextureMap.GetIndexMatTex(Map[cnt].sBelowTempConvertTo.getData(), nullptr, true, FormatString("BelowTempConvertTo of mat %s", Map[cnt].Name).getData());
		if (Map[cnt].sAboveTempConvertTo.getLength())
			Map[cnt].AboveTempConvertTo = Game.TextureMap.GetIndexMatTex(Map[cnt].sAboveTempConvertTo.getData(), nullptr, true, FormatString("AboveTempConvertTo of mat %s", Map[cnt].Name).getData());
	}
}

#endif

void C4MaterialMap::SetMatReaction(int32_t iPXSMat, int32_t iLSMat, C4MaterialReaction *pReact)
{
	// evaluate reaction swap
	if (pReact && pReact->fReverse) std::swap(iPXSMat, iLSMat);
	// set it
	ppReactionMap[(iLSMat + 1) * (Num + 1) + iPXSMat + 1] = pReact;
}

bool C4MaterialMap::SaveEnumeration(CppC4Group &group)
{
	auto *mapbuf = static_cast<char *>(malloc(1000 * sizeof(char)));
	mapbuf[0] = 0;
	SAppend("[Enumeration]", mapbuf); SAppend(LineFeed, mapbuf);
	for (int32_t cnt = 0; cnt < Num; cnt++)
	{
		SAppend(Map[cnt].Name, mapbuf);
		SAppend(LineFeed, mapbuf);
	}
	SAppend(EndOfFile, mapbuf);

	return CppC4Group_Add(group, C4CFN_MatMap, mapbuf, strlen(mapbuf));
}

bool C4MaterialMap::LoadEnumeration(CppC4Group &group)
{
	// Load enumeration map (from savegame), succeed if not present
	std::string data;
	if (!CppC4Group_LoadEntryString(group, C4CFN_MatMap, data))
	{
		return true;
	}

	// Sort material array by enumeration map, fail if some missing
	const char *csearch;
	char cmatname[C4M_MaxName + 1];
	int32_t cmat = 0;
	if (!(csearch = SSearch(data.c_str(), "[Enumeration]"))) { return false; }
	csearch = SAdvanceSpace(csearch);
	while (IsIdentifier(*csearch))
	{
		SCopyIdentifier(csearch, cmatname, C4M_MaxName);
		if (!SortEnumeration(cmat, cmatname))
		{
			// Output error message!
			return false;
		}
		cmat++;
		csearch += SLen(cmatname);
		csearch = SAdvanceSpace(csearch);
	}

	return true;
}

bool C4MaterialMap::SortEnumeration(int32_t iMat, const char *szMatName)
{
	// Not enough materials loaded
	if (iMat >= Num) return false;

	// Find requested mat
	int32_t cmat;
	for (cmat = iMat; cmat < Num; cmat++)
		if (SEqual(szMatName, Map[cmat].Name))
			break;
	// Not found
	if (cmat >= Num) return false;

	// already the same?
	if (cmat == iMat) return true;

	// Move requested mat to indexed position
	C4Material mswap;
	mswap = Map[iMat];
	Map[iMat] = Map[cmat];
	Map[cmat] = mswap;

	return true;
}

void C4MaterialMap::Default()
{
	Num = 0;
	Map = nullptr;
	ppReactionMap = nullptr;
}

#ifdef C4ENGINE

bool mrfInsertCheck(int32_t &iX, int32_t &iY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, bool *pfPosChanged)
{
	// always manipulating pos/speed here
	if (pfPosChanged) *pfPosChanged = true;

	// Rough contact? May splash
	if (fYDir > itofix(1))
		if (Game.Material.Map[iPxsMat].SplashRate && !Random(Game.Material.Map[iPxsMat].SplashRate))
		{
			fYDir = -fYDir / 8;
			fXDir = fXDir / 8 + FIXED100(Random(200) - 100);
			if (fYDir) return false;
		}

	// Contact: Stop
	fYDir = 0;

	// Incindiary mats smoke on contact even before doing their slide
	if (Game.Material.Map[iPxsMat].Incindiary)
		if (!Random(25)) Smoke(iX, iY, 4 + Rnd3());

	// Move by mat path/slide
	int32_t iSlideX = iX, iSlideY = iY;
	if (Game.Landscape.FindMatSlide(iSlideX, iSlideY, Sign(GravAccel), Game.Material.Map[iPxsMat].Density, Game.Material.Map[iPxsMat].MaxSlide))
	{
		if (iPxsMat == iLsMat)
		{
			iX = iSlideX; iY = iSlideY; fXDir = 0; return false;
		}
		// Accelerate into the direction
		fXDir = (fXDir * 10 + Sign(iSlideX - iX)) / 11 + FIXED10(Random(5) - 2);
		// Slide target in range? Move there directly.
		if (Abs(iX - iSlideX) <= Abs(fixtoi(fXDir)))
		{
			iX = iSlideX;
			iY = iSlideY;
			if (fYDir <= 0) fXDir = 0;
		}
		// Continue existance
		return false;
	}
	// insertion OK
	return true;
}

bool mrfUserCheck(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	// check execution mask
	if ((1 << evEvent) & ~pReaction->iExecMask) return false;
	// do splash/slide check, if desired
	if (pReaction->fInsertionCheck && evEvent == meePXSMove)
		if (!mrfInsertCheck(iX, iY, fXDir, fYDir, iPxsMat, iLsMat, pfPosChanged))
			return false;
	// checks OK; reaction may be applied
	return true;
}

bool C4MaterialMap::mrfConvert(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	if (pReaction->fUserDefined) if (!mrfUserCheck(pReaction, iX, iY, iLSPosX, iLSPosY, fXDir, fYDir, iPxsMat, iLsMat, evEvent, pfPosChanged)) return false;
	switch (evEvent)
	{
	case meePXSMove: // PXS movement
		// for hardcoded stuff: only InMatConvert is Snow in Water, which does not have any collision proc
		if (!pReaction->fUserDefined) break;
		// user-defined conversions may also convert upon hitting materials

	case meePXSPos: // PXS check before movement
	{
		// Check depth
		int32_t iDepth = pReaction->fUserDefined ? pReaction->iDepth : Game.Material.Map[iPxsMat].InMatConvertDepth;
		if (!iDepth || GBackMat(iX, iY - iDepth) == iLsMat)
		{
			// Convert
			iPxsMat = pReaction->fUserDefined ? pReaction->iConvertMat : Game.Material.Map[iPxsMat].InMatConvertTo;
			if (!MatValid(iPxsMat))
				// Convert failure (target mat not be loaded, or target may be C4TLS_MatSky): Kill Pix
				return true;
			// stop movement after conversion
			fXDir = fYDir = 0;
			if (pfPosChanged) *pfPosChanged = true;
		}
	}
	break;

	case meeMassMove: // MassMover-movement
		// Conversion-transfer to PXS
		Game.PXS.Create(iPxsMat, itofix(iX), itofix(iY));
		return true;
	}
	// not handled
	return false;
}

bool C4MaterialMap::mrfPoof(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	if (pReaction->fUserDefined) if (!mrfUserCheck(pReaction, iX, iY, iLSPosX, iLSPosY, fXDir, fYDir, iPxsMat, iLsMat, evEvent, pfPosChanged)) return false;
	switch (evEvent)
	{
	case meeMassMove: // MassMover-movement
	case meePXSPos: // PXS check before movement: Kill both landscape and PXS mat
		Game.Landscape.ExtractMaterial(iLSPosX, iLSPosY);
		if (!Rnd3()) Smoke(iX, iY, 3);
		if (!Rnd3()) StartSoundEffectAt("Pshshsh", iX, iY);
		return true;

	case meePXSMove: // PXS movement
		// incindiary/extinguisher/corrosives are always same density proc; so do insertion check first
		if (!pReaction->fUserDefined)
			if (!mrfInsertCheck(iX, iY, fXDir, fYDir, iPxsMat, iLsMat, pfPosChanged))
				// either splash or slide prevented interaction
				return false;
		// Always kill both landscape and PXS mat
		Game.Landscape.ExtractMaterial(iLSPosX, iLSPosY);
		if (!Rnd3()) Smoke(iX, iY, 3);
		if (!Rnd3()) StartSoundEffectAt("Pshshsh", iX, iY);
		return true;
	}
	// not handled
	return false;
}

bool C4MaterialMap::mrfCorrode(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	if (pReaction->fUserDefined) if (!mrfUserCheck(pReaction, iX, iY, iLSPosX, iLSPosY, fXDir, fYDir, iPxsMat, iLsMat, evEvent, pfPosChanged)) return false;
	switch (evEvent)
	{
	case meePXSPos: // PXS check before movement
		// No corrosion - it would make acid incredibly effective
		break;
	case meeMassMove: // MassMover-movement
	{
		// evaluate corrosion percentage
		bool fDoCorrode;
		if (pReaction->fUserDefined)
			fDoCorrode = (Random(100) < pReaction->iCorrosionRate);
		else
			fDoCorrode = (Random(100) < Game.Material.Map[iPxsMat].Corrosive) && (Random(100) < Game.Material.Map[iLsMat].Corrode);
		if (fDoCorrode)
		{
			ClearBackPix(iLSPosX, iLSPosY);
			if (!Random(5)) Smoke(iX, iY, 3 + Random(3));
			if (!Random(20)) StartSoundEffectAt("Corrode", iX, iY);
			return true;
		}
	}
	break;

	case meePXSMove: // PXS movement
	{
		// corrodes to corrosives are always same density proc; so do insertion check first
		if (!pReaction->fUserDefined)
			if (!mrfInsertCheck(iX, iY, fXDir, fYDir, iPxsMat, iLsMat, pfPosChanged))
				// either splash or slide prevented interaction
				return false;
		// evaluate corrosion percentage
		bool fDoCorrode;
		if (pReaction->fUserDefined)
			fDoCorrode = (Random(100) < pReaction->iCorrosionRate);
		else
			fDoCorrode = (Random(100) < Game.Material.Map[iPxsMat].Corrosive) && (Random(100) < Game.Material.Map[iLsMat].Corrode);
		if (fDoCorrode)
		{
			ClearBackPix(iLSPosX, iLSPosY);
			Game.Landscape.CheckInstabilityRange(iLSPosX, iLSPosY);
			if (!Random(5)) Smoke(iX, iY, 3 + Random(3));
			if (!Random(20)) StartSoundEffectAt("Corrode", iX, iY);
			return true;
		}
		// Else: dead. Insert material here
		Game.Landscape.InsertMaterial(iPxsMat, iX, iY);
		return true;
	}
	}
	// not handled
	return false;
}

bool C4MaterialMap::mrfIncinerate(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	// not available as user reaction
	assert(!pReaction->fUserDefined);
	switch (evEvent)
	{
	case meeMassMove: // MassMover-movement
	case meePXSPos: // PXS check before movement
		if (Game.Landscape.Incinerate(iX, iY)) return true;
		break;

	case meePXSMove: // PXS movement
		// incinerate to inflammables are always same density proc; so do insertion check first
		if (!mrfInsertCheck(iX, iY, fXDir, fYDir, iPxsMat, iLsMat, pfPosChanged))
			// either splash or slide prevented interaction
			return false;
		// evaluate inflammation (should always succeed)
		if (Game.Landscape.Incinerate(iX, iY)) return true;
		// Else: dead. Insert material here
		Game.Landscape.InsertMaterial(iPxsMat, iX, iY);
		return true;
	}
	// not handled
	return false;
}

bool C4MaterialMap::mrfInsert(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	if (pReaction->fUserDefined) if (!mrfUserCheck(pReaction, iX, iY, iLSPosX, iLSPosY, fXDir, fYDir, iPxsMat, iLsMat, evEvent, pfPosChanged)) return false;
	switch (evEvent)
	{
	case meePXSPos: // PXS check before movement
		break;

	case meePXSMove: // PXS movement
	{
		// check for bounce/slide
		if (!pReaction->fUserDefined)
			if (!mrfInsertCheck(iX, iY, fXDir, fYDir, iPxsMat, iLsMat, pfPosChanged))
				// continue existing
				return false;
		// Else: dead. Insert material here
		Game.Landscape.InsertMaterial(iPxsMat, iX, iY);
		return true;
	}

	case meeMassMove: // MassMover-movement
		break;
	}
	// not handled
	return false;
}

bool C4MaterialMap::mrfScript(C4MaterialReaction *pReaction, int32_t &iX, int32_t &iY, int32_t iLSPosX, int32_t iLSPosY, FIXED &fXDir, FIXED &fYDir, int32_t &iPxsMat, int32_t iLsMat, MaterialInteractionEvent evEvent, bool *pfPosChanged)
{
	// do generic checks for user-defined reactions
	if (!mrfUserCheck(pReaction, iX, iY, iLSPosX, iLSPosY, fXDir, fYDir, iPxsMat, iLsMat, evEvent, pfPosChanged))
		return false;

	// check script func
	if (!pReaction->pScriptFunc) return false;
	// OK - let's call it!
	//               0           1           2                3                4                                    5                                    6                7               8
	int32_t iXDir1, iYDir1, iXDir2, iYDir2;
	C4AulParSet pars(C4VInt(iX), C4VInt(iY), C4VInt(iLSPosX), C4VInt(iLSPosY), C4VInt(iXDir1 = fixtoi(fXDir, 100)), C4VInt(iYDir1 = fixtoi(fYDir, 100)), C4VInt(iPxsMat), C4VInt(iLsMat), C4VInt(evEvent));
	if (!!pReaction->pScriptFunc->Exec(nullptr, &pars, false))
	{
		// PXS shall be killed!
		return true;
	}
	// PXS shall exist further: write back parameters
	iPxsMat = pars[6].getInt();
	int32_t iX2 = pars[0].getInt(), iY2 = pars[1].getInt();
	iXDir2 = pars[4].getInt(); iYDir2 = pars[5].getInt();
	if (iX != iX2 || iY != iY2 || iXDir1 != iXDir2 || iYDir1 != iYDir2)
	{
		// changes to pos/speed detected
		if (pfPosChanged) *pfPosChanged = true;
		iX = iX2; iY = iY2;
		fXDir = FIXED100(iXDir2);
		fYDir = FIXED100(iYDir2);
	}
	// OK; done
	return false;
}

void C4MaterialMap::UpdateScriptPointers()
{
	// update in all materials
	for (int32_t i = 0; i < Num; ++i) Map[i].UpdateScriptPointers();
}

#endif
