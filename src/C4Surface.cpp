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

/* Extension to CSurface that handles bitmaps in C4Group files */

#include <C4Include.h>
#include <C4Surface.h>
#include <C4GroupSet.h>

#include <C4Group.h>
#include <C4Log.h>

#include <Bitmap256.h>
#include <StdBitmap.h>
#include <StdJpeg.h>
#include <StdPNG.h>
#include <StdDDraw2.h>

#include "cppc4group.hpp"

#include <cstdint>
#include <memory>
#include <stdexcept>

bool C4Surface::LoadAny(CppC4Group &group, const std::string &filePath, bool fOwnPal, bool fNoErrIfNotFound)
{
	// Entry name
	char szFilename[_MAX_PATH + 1];
	SCopy(filePath.c_str(), szFilename, _MAX_PATH);
	char *szExt = GetExtension(szFilename);
	if (!*szExt)
	{
		// no extension: Default to extension that is found as file in group
		const char *const extensions[] = { "png", "bmp", "jpeg", "jpg", nullptr };
		int i = 0; const char *szExt;
		while (szExt = extensions[i++])
		{
			EnforceExtension(szFilename, szExt);
			if (group.getEntryInfo(szFilename)) break;
		}
	}
	// Load surface
	return Load(group, szFilename, fOwnPal, fNoErrIfNotFound);
}

bool C4Surface::LoadAny(C4GroupSet &hGroupset, const char *szName, bool fOwnPal, bool fNoErrIfNotFound)
{
	// Entry name
	char szFilename[_MAX_FNAME + 1];
	SCopy(szName, szFilename, _MAX_FNAME);
	char *szExt = GetExtension(szFilename);
	CppC4Group *pGroup = nullptr;
	if (!*szExt)
	{
		// no extension: Default to extension that is found as file in group
		const char *const extensions[] = { "png", "bmp", "jpeg", "jpg", nullptr };
		int i = 0; const char *szExt;
		while (szExt = extensions[i++])
		{
			EnforceExtension(szFilename, szExt);
			std::tie(pGroup, std::ignore) = hGroupset.FindEntry(szFilename);
			if (pGroup) break;
		}
	}
	if (!pGroup) return false;
	// Load surface
	return Load(*pGroup, szFilename, fOwnPal, fNoErrIfNotFound);
}

bool C4Surface::Load(CppC4Group &group, const std::string &filePath, bool fOwnPal, bool fNoErrIfNotFound)
{
	if (!group.getEntryInfo(filePath))
	{
		// file not found
		if (!fNoErrIfNotFound) LogF("%s: %s", LoadResStr("IDS_PRC_FILENOTFOUND"), filePath.c_str());
		return false;
	}
	// determine file type by file extension and load accordingly
	bool fSuccess;
	if (SEqualNoCase(GetExtension(filePath.c_str()), "png"))
		fSuccess = !!ReadPNG(group, filePath);
	else if (SEqualNoCase(GetExtension(filePath.c_str()), "jpeg")
		|| SEqualNoCase(GetExtension(filePath.c_str()), "jpg"))
		fSuccess = ReadJPEG(group, filePath);
	else
		fSuccess = !!Read(group, filePath, fOwnPal);
	// loading error? log!
	if (!fSuccess)
		LogF("%s: %s", LoadResStr("IDS_ERR_NOFILE"), filePath.c_str());
	// done, success
	return fSuccess;
}

bool C4Surface::ReadPNG(CppC4Group &group, const std::string &filePath)
{
	auto data = group.getEntryData(filePath);

	// load as png file
	std::unique_ptr<StdBitmap> bmp;
	std::uint32_t width, height; bool useAlpha;
	try
	{
		CPNGFile png{data->data, data->size};
		width = png.Width(); height = png.Height(), useAlpha = png.UsesAlpha();
		bmp.reset(new StdBitmap(width, height, useAlpha));
		png.Decode(bmp->GetBytes());
	}
	catch (const std::runtime_error &e)
	{
		LogF("Could not create surface from PNG file: %s", e.what());
		bmp.reset();
	}
	// abort if loading wasn't successful
	if (!bmp) return false;
	// create surface(s) - do not create an 8bit-buffer!
	if (!Create(width, height)) return false;
	// lock for writing data
	if (!Lock()) return false;
	if (!ppTex)
	{
		Unlock();
		return false;
	}
	// write pixels
	for (int tY = 0; tY * iTexSize < Hgt; ++tY) for (int tX = 0; tX * iTexSize < Wdt; ++tX)
	{
		assert(tX >= 0 && tY >= 0 && tX < iTexX && tY < iTexY);
		// Get Texture and lock it
		CTexRef *pTexRef = *(ppTex + tY * iTexX + tX);
		if (!pTexRef->Lock()) continue;
		// At the edges, not the whole texture is used
		int maxY = (std::min)(iTexSize, Hgt - tY * iTexSize), maxX = (std::min)(iTexSize, Wdt - tX * iTexSize);
		for (int iY = 0; iY < maxY; ++iY)
		{
			// The global, not texture-relative position
			int rY = iY + tY * iTexSize;
#ifndef __BIG_ENDIAN__
			if (useAlpha)
			{
				// Optimize the easy case of a png in the same format as the display
				// 32 bit
				uint32_t *pPix = (uint32_t *)(((char *)pTexRef->texLock.pBits) + iY * pTexRef->texLock.Pitch);
				memcpy(pPix, static_cast<const std::uint32_t *>(bmp->GetPixelAddr32(0, rY)) +
					tX * iTexSize, maxX * 4);
				int iX = maxX;
				while (iX--) { if (((uint8_t *)pPix)[3] == 0xff) *pPix = 0xff000000; ++pPix; }
			}
			else
#endif
			{
				// Loop through every pixel and convert
				for (int iX = 0; iX < maxX; ++iX)
				{
					uint32_t dwCol = bmp->GetPixel(iX + tX * iTexSize, rY);
					// if color is fully transparent, ensure it's black
					if (dwCol >> 24 == 0xff) dwCol = 0xff000000;
					// set pix in surface
					uint32_t *pPix = (uint32_t *)(((char *)pTexRef->texLock.pBits) + iY * pTexRef->texLock.Pitch + iX * 4);
					*pPix = dwCol;
				}
			}
		}
		pTexRef->Unlock();
	}
	// unlock
	Unlock();
	// Success
	return true;
}

bool C4Surface::SavePNG(CppC4Group &group, const std::string &filePath, bool fSaveAlpha, bool fApplyGamma, bool fSaveOverlayOnly)
{
	// Using temporary file at C4Group temp path
	char szTemp[_MAX_PATH + 1];
	SCopy(C4Group_GetTempPath(), szTemp);
	SAppend(GetFilename(filePath.c_str()), szTemp);
	MakeTempFilename(szTemp);
	// Save to temporary file
	if (!CSurface::SavePNG(szTemp, fSaveAlpha, fApplyGamma, fSaveOverlayOnly)) return false;
	// Move temp file to group
	if (!group.addFromDisk(szTemp, filePath)) return false;
	// Success
	return true;
}

bool C4Surface::Copy(C4Surface &fromSfc)
{
	// Clear anything old
	Clear();
	// Default to other surface's color depth
	Default();
	// Create surface
	if (!Create(fromSfc.Wdt, fromSfc.Hgt)) return false;
	// Blit copy
	if (!lpDDraw->BlitSurface(&fromSfc, this, 0, 0, false))
	{
		Clear(); return false;
	}
	// Success
	return true;
}

bool C4Surface::ReadJPEG(CppC4Group &group, const std::string &filePath)
{
	auto data = group.getEntryData(filePath);
	bool locked = false;
	try
	{
		StdJpeg jpeg(data->data, data->size);
		const std::uint32_t width = jpeg.Width(), height = jpeg.Height();

		// create surface(s) - do not create an 8bit-buffer!
		if (!Create(width, height)) return false;

		// lock for writing data
		if (!Lock()) return false;
		locked = true;

		// put the data in the image
		for (std::uint32_t y = 0; y < height; ++y)
		{
			const auto row = jpeg.DecodeRow();
			for (std::uint32_t x = 0; x < width; ++x)
			{
				const auto pixel = static_cast<const uint8_t *>(row) + x * 3;
				SetPixDw(x, y, C4RGB(pixel[0], pixel[1], pixel[2]));
			}
		}
		jpeg.Finish();
	}
	catch (const std::runtime_error &e)
	{
		LogF("Could not create surface from JPEG file: %s", e.what());
	}

	// unlock
	if (locked) Unlock();
	// return if successful
	return true;
}
